package calc

import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec
import cats.implicits._

object Calculator {
  trait Op
  object Plus extends Op { override def toString: String = "+" }
  object Sub  extends Op { override def toString: String = "-" }
  object Mult extends Op { override def toString: String = "*" }
  object Div  extends Op { override def toString: String = "/" }
  val ops = List(Plus, Sub, Mult, Div)

  type EvalElem = Either[Double, Op]

  val emptyInput                 = new Exception(s"cannot run computation on empty input")
  def invalidElem(elem: String)  = new Exception(s"$elem is not a number or one of the following operators ${ops.mkString(", ")}")
  def invalidSeq(seq: String)    = new Exception(s"$seq is not a valid sequence")
  val emptyTree                  = new Exception(s"cannot run computation on empty input")
  def missingRightInput(op: Op)  = new Exception(s"operator $op missing right-hand input")

  def run(input: String): Try[Double] = for {
    elems  <- lex(input)
    tree   <- parse(elems)
    result <- eval(tree)
  } yield result

  def lex(input: String): Try[List[EvalElem]] =
    input.split(' ').toList.map {
      case ""  => Failure(emptyInput)
      case "+" => Success(Right(Plus))
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

      case (Left(num) :: _, Literal(lit)) =>
        Failure(invalidSeq(s"$lit $num"))

      case (Left(num) :: tail, Empty) =>
        go(tail, Literal(num))

      case (Left(num) :: tail, op @ Operator(_, (in0, Empty))) =>
        go(tail, Operator(op.value, (in0, Literal(num))))

      case (Right(op) :: tail, cTree) => go(tail, Operator(op, (cTree, Empty)))
    }

    go(input, Empty)
  }

  private[calc] def eval(parsed: CalcTree): Try[Double] = ???

}
