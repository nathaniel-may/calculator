package calc

import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec
import cats.implicits._
import zipper._

object Calculator {
  sealed trait Order0Op
  sealed trait Order1Op
  sealed trait Op
  object Add  extends Op with Order1Op { override def toString: String = "+" }
  object Sub  extends Op with Order1Op { override def toString: String = "-" }
  object Mult extends Op with Order0Op { override def toString: String = "*" }
  object Div  extends Op with Order0Op { override def toString: String = "/" }
  val ops = List(Add, Sub, Mult, Div)

  type EvalElem = Either[Double, Op]

  implicit val unzippableOps: Unzip[CalcTree] = new Unzip[CalcTree] {
    override def unzip(node: CalcTree): List[CalcTree] = node match {
      case Empty               => Nil
      case Literal(_)          => Nil
      case Operator(_, (l, r)) => List(l, r)
    }

    override def zip(node: CalcTree, children: List[CalcTree]): CalcTree = (node, children) match {
      case (Empty,           _)           => Empty
      case (Literal(_),      _)           => Empty
      case (Operator(op, _), l :: r :: _) => Operator(op, (l, r))
      case _                              => Empty
    }
  }

  class CalcCompilationException(message: String) extends Exception(message)
  class CalcRuntimeException(message: String)     extends Exception(message)

  val boom                       = new CalcCompilationException("unknown compilation error") // TODO replace this with something more sane
  val emptyInput                 = new CalcCompilationException("cannot run computation on empty input")
  def invalidElem(elem: String)  = new CalcCompilationException(s"$elem is not a number or one of the following operators ${ops.mkString(", ")}")
  def missingLeftInput(op: Op)   = new CalcCompilationException(s"cannot start input with an operator: started with $op")
  def invalidSeq(seq: String)    = new CalcCompilationException(s"$seq is not a valid sequence")
  val emptyTree                  = new CalcCompilationException("cannot run computation on empty input")
  def missingRightInput(op: Op)  = new CalcCompilationException(s"operator $op missing right-hand input")
  val divByZero                  = new CalcRuntimeException("cannot divide by zero")

  def run(input: String): Try[Double] = for {
    elems  <- lex(input)
    tree   <- parse(elems)
    result <- eval(tree)
  } yield result

  private[calc] def lex(input: String): Try[List[CalcTree]] =
    input.split(' ').toList.map {
      case ""  => Failure(emptyInput)
      case "+" => Success(Operator(Add,  (Empty, Empty)))
      case "-" => Success(Operator(Sub,  (Empty, Empty)))
      case "*" => Success(Operator(Mult, (Empty, Empty)))
      case "/" => Success(Operator(Div, (Empty, Empty)))
      case num => Try(num.toDouble)
        .fold(_ => Failure(invalidElem(num)), double => Success(Literal(double)))
    }
      .sequence

  private[calc] def parse(input: List[CalcTree]): Try[CalcTree] = {
    def validate(in: List[CalcTree], err: Try[Unit]): Try[Unit] = in match {
      case Nil =>
        err

      case Empty :: _ =>
        Failure(emptyInput)

      case _ :: Empty :: _ =>
        Failure(emptyInput)

      case Operator(op0, _) :: Operator(op1, _) :: _ =>
        Failure(invalidSeq(s"$op0 $op1"))

      case Literal(l0) :: Literal(l1) :: _ =>
        Failure(invalidSeq(s"$l0 $l1"))

      case Operator(op, _) :: Nil =>
        Failure(missingRightInput(op))

      case Literal(_) :: tail =>
        validate(tail, err)

      case Operator(_, _) :: tail =>
        validate(tail, err)
    }

    @tailrec
    def pushdown(in: List[CalcTree], out: List[Operator]): Try[List[Operator]] = in match {
      case Nil =>
        Success(out.reverse)

      case Literal(l) :: Operator(op: Order0Op, _) :: Literal(r) :: tail =>
        pushdown(tail, Operator(op, (Literal(l), Literal(r))) :: out)

      case (op @ Operator(_: Order1Op, _)) :: tail =>
        pushdown(tail, op :: out)

        // I actually want to pattern match against op "not of type Order0Op"
      case (l @ Literal(_)) :: Operator(op: Order1Op, _) :: tail =>
        pushdown(tail, Operator(op, (l, Empty)) :: out)

      case _ =>
        Failure(boom)
    }

    def stitch(in: List[Operator]): CalcTree = {
      @tailrec
      def AddToRoot(root: Zipper[CalcTree], child: CalcTree): Option[CalcTree] = {
        Try(root.moveDownRight) match {
          case Success(zNext) if (zNext.focus match {
            case Empty => true;
            case _ => false
          }) => Some(zNext.update(_ => child).commit)

          case Success(zNext) => AddToRoot(zNext, child)

          case _ => None
        }
      }

      @tailrec
      def AddToChild(root: CalcTree, child: Zipper[CalcTree]): Option[CalcTree] = {
        Try(child.moveDownLeft) match {
          case Success(zNext) if (zNext.focus match {
            case Empty => true;
            case _ => false
          }) => Some(zNext.update(_ => root).commit)

          case Success(zNext) => AddToChild(root, zNext)

          case _ => None
        }
      }

      in.drop(1).foldLeft[CalcTree](in.head) { (tree, op) =>
        AddToRoot(Zipper(tree), op)
          .fold(AddToChild(tree, Zipper(op)))(Some(_))
          .get // TODO
      }
    }

    for {
      _   <- validate(input, Success(()))
      ops <- pushdown(input, List())
    } yield stitch(ops)
  }

  //TODO deal with results that are outside the double representation
  private[calc] def eval(parsed: CalcTree): Try[Double] = parsed match {
    case Empty        => Failure(new Exception("Internal error: attempted to evaluate empty parse tree"))
    case Literal(num) => Success(num)
    case Operator(op, (lTree, rTree)) => for {
      l      <- eval(lTree)
      r      <- eval(rTree)
      math   =  op match {
                  case Add  => l + r
                  case Sub  => l - r
                  case Mult => l * r
                  case Div  => l / r
                }
      result <- if (math == Double.NegativeInfinity || math == Double.PositiveInfinity)
                  Failure(divByZero)
                else
                  Success(math)
    } yield result
  }

}
