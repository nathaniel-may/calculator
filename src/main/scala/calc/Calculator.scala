package calc

import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec
import scala.math.BigDecimal
import cats.implicits._
import Exceptions._

object Calculator {

  object Priority extends Enumeration {
    type Priority = Value
    val Second, First = Value
  }

  sealed trait Op {
    val priority: Priority.Priority
  }

  object Mult extends Op {
    val priority = Priority.First
    override def toString: String = "*"
  }

  object Div  extends Op {
    val priority = Priority.First
    override def toString: String = "/"
  }

  object Add  extends Op {
    val priority = Priority.Second
    override def toString: String = "+"
  }

  object Sub  extends Op {
    val priority = Priority.Second
    override def toString: String = "-"
  }

  val ops = List(Add, Sub, Mult, Div)

  sealed trait Tok
  case class TNum(value: BigDecimal) extends Tok
  case class TOp(value: Op)          extends Tok

  def run(input: String): Try[BigDecimal] = for {
    elems  <- Lexer.run(input)
    valid  <- Parser.run(elems)
    result <- eval(valid)
  } yield result

  def eval(parsed: ParseTree): Try[BigDecimal] = {
    def runOp(op: Op, args: (BigDecimal, BigDecimal)): Try[BigDecimal] = (op, args) match {
      case (Add,  (l, r))            => Success(l + r)
      case (Sub,  (l, r))            => Success(l - r)
      case (Mult, (l, r))            => Success(l * r)
      case (Div,  (l, r)) if r != 0  => Success(l / r)
      case (Div,  (_, _))            => Failure(new DivideByZeroErr)
    }

    // stack overflow possible
    parsed match {
      case PNum(answer) =>
        Success(answer)

      case POp(op, param0, param1) =>
        for {
          p0 <- eval(param0)
          p1 <- eval(param1)
          ans <- runOp(op, (p0, p1))
        } yield ans
    }
  }
}
