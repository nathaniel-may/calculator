package calc

import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec
import cats.implicits._

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
    @tailrec
    def pass0(in: List[CalcTree], out: List[CalcTree]): Try[List[CalcTree]] = in match {
      case Nil =>
        Success(out.reverse)

      case Operator(op0, _) :: Operator(op1, _) :: _ =>
        Failure(invalidSeq(s"$op0 $op1"))

      case Literal(l0) :: Literal(l1) :: _ =>
        Failure(invalidSeq(s"$l0 $l1"))

      case Literal(l) :: Operator(op: Order0Op, _) :: Literal(r) :: tail =>
        pass0(tail, Operator(op, (Literal(l), Literal(r))) :: out)

      case Operator(op: Order1Op, (l, r)) :: tail =>
        pass0(tail, Operator(op, (l, r)) :: out)

      case _ =>
        Failure(boom)
    }

    def pass1(in: List[CalcTree], out: List[CalcTree]): Try[List[CalcTree]] = in match {
      case Nil =>
        Success(out.reverse)

      case Literal(l) :: Operator(op, (Empty, Empty)) :: Literal(r) :: tail =>
        pass1(tail, Operator(op, (Literal(l), Literal(r))) :: out)

      case (l @ Operator(_, _)) :: Operator(op: Order1Op, _) :: (r @ Literal(_)) :: tail =>
        pass1(tail, Operator(op, (l, r)) :: out)

      case (l @ Literal(_)) :: Operator(op: Order1Op, _) :: (r @ Operator(_, _)) :: tail =>
        pass1(tail, Operator(op, (l, r)) :: out)

      case (l @ Operator(_, _)) :: Operator(op: Order1Op, _) :: (r @ Operator(_, _)) :: tail =>
        pass1(tail, Operator(op, (l, r)) :: out)

      case _ =>
        Failure(boom)
    }

    def finalPass(in: List[CalcTree], out: CalcTree): Try[CalcTree] = (in, out) match {
      case (Nil, outTree) =>
        Success(outTree)

      case (h :: t, Empty) =>
        finalPass(t, h)

      case (h :: t, Operator(op, (Empty, r))) =>
        finalPass(t, Operator(op, (h, r)))

      case (Operator(op, (l, Empty)) :: t, outTree) =>
        finalPass(t, Operator(op, (l, outTree)))

      case _ =>
        Failure(boom)
    }

    for {
      pass0 <- pass0(input, List())
      pass1 <- pass1(pass0, List())
      all   <- finalPass(pass1, Empty)
    } yield all
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
