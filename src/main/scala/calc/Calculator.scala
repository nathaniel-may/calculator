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
    valid  <- parse(elems)
    result <- eval(valid)
  } yield result

  // uses shunting-yard algorithm to build a parse tree. Could build BNF instead, but this is more generic.
  // https://en.wikipedia.org/wiki/Shunting-yard_algorithm
  private[calc] def parse(input: List[Tok]): Try[ParseTree] = {
    @tailrec
    def go(input: List[Tok], parseTree: List[ParseTree], shuntStack: List[TOp]): Try[ParseTree] = (input, parseTree, shuntStack) match {
      case (Nil, res ::  Nil, Nil) =>
        Success(res)

      case (TOp(op) :: Nil, Nil, Nil) =>
        Failure(MissingLeftInputErr.from(op))

      case (TNum(num0) :: TNum(num1) :: _, _, _) =>
        Failure(InvalidSequenceErr.from(s"$num0 $num1"))

      case (TOp(op0) :: TOp(op1) :: _, _, _) =>
        Failure(InvalidSequenceErr.from(s"$op0 $op1"))

      case (TOp(op) :: Nil, _, _) =>
        Failure(MissingRightInputErr.from(op))

      case (Nil, a :: b :: treeTail, TOp(op) :: shuntTail) =>
        go(Nil, POp(op, b, a) :: treeTail, shuntTail)

      case (TNum(num) :: tail, tree, stack) =>
        go(tail, PNum(num) :: tree, stack)

      case (TOp(op) :: tail, tree, Nil) =>
        go(tail, tree, TOp(op) :: Nil)

      case (TOp(op) :: tail, tree, stack @ TOp(shunted) :: Nil)
        if shunted.priority < op.priority =>
        go(tail, tree, TOp(op) :: stack)

      case (toks @ TOp(op) :: _, a :: b :: treeTail, TOp(shunted) :: shuntTail)
        if shunted.priority >= op.priority =>
        go(toks, POp(shunted, b, a) :: treeTail, shuntTail)

      case _ =>
        Failure(new UnknownCompilationErr)
    }

    go(input, List.empty, List.empty)
  }

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
