package calc

import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec
import cats.implicits._

object Calculator {
  trait Op
  object Add  extends Op { override def toString: String = "+" }
  object Sub  extends Op { override def toString: String = "-" }
  object Mult extends Op { override def toString: String = "*" }
  object Div  extends Op { override def toString: String = "/" }
  val ops = List(Add, Sub, Mult, Div)

  type EvalElem = Either[Double, Op]

  class CalcCompilationException(message: String) extends Exception(message)
  class CalcRuntimeException(message: String)     extends Exception(message)

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

  private[calc] def lex(input: String): Try[List[EvalElem]] =
    input.split(' ').toList.map {
      case ""  => Failure(emptyInput)
      case "+" => Success(Right(Add))
      case "-" => Success(Right(Sub))
      case "*" => Success(Right(Mult))
      case "/" => Success(Right(Div))
      case num => Try(num.toDouble)
        .fold(_ => Failure(invalidElem(num)), double => Success(Left(double)))
    }
      .sequence

  private[calc] def parse(input: List[EvalElem]): Try[CalcTree] = {
    @tailrec
    def go(typed: List[EvalElem], tree: CalcTree): Try[CalcTree] = (typed, tree) match {
      case (Nil, Empty) =>
        Failure(emptyTree)

      case (Nil, Operator(op, (_, Empty))) =>
        Failure(missingRightInput(op))

      case (Nil, cTree @ _) =>
        Success(cTree)

      case (Right(op) :: _, Empty) =>
        Failure(missingLeftInput(op))

      case (Right(op1) :: _, Operator(op0, (_, Empty))) =>
        Failure(invalidSeq(s"$op0 $op1"))

      case (Right(op) :: tail, cTree) =>
        go(tail, Operator(op, (cTree, Empty)))

      case (Left(num) :: _, Literal(lit)) =>
        Failure(invalidSeq(s"$lit $num"))

      case (Left(num) :: tail, Empty) =>
        go(tail, Literal(num))

      case (Left(num) :: tail, op @ Operator(_, (in0, Empty))) =>
        go(tail, Operator(op.value, (in0, Literal(num))))

      case (Left(num) :: _, Operator(_, (_, in1))) =>
        Failure(invalidSeq(s"$num $in1"))
    }

    go(input, Empty)
  }

  //TODO deal with results that are outside the double representation
  private[calc] def eval(parsed: CalcTree): Try[Double] = parsed match {
    case Empty        => Failure(new Exception("Internal error: attempted to evaluate empty parse tree"))
    case Literal(num) => Success(num)
    case Operator(op, (lTree, rTree)) => for {
      l      <- eval(lTree)
      r      <- eval(rTree)
      result <- op match {
                  case Add  => Success(l + r)
                  case Sub  => Success(l - r)
                  case Mult => Success(l * r)
                  case Div  => Try(l / r).fold(_ => Failure(divByZero), Success(_))
                }
    } yield result
  }

}
