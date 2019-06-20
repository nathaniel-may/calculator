package calc

import scala.util.{Try, Success, Failure}
import cats.implicits._

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
  case class TNum(value: Double) extends Tok
  case class TOp(value: Op)      extends Tok

  class CalcCompilationException(message: String) extends Exception(message)
  class CalcRuntimeException(message: String)     extends Exception(message)

  // Some exception messages require triple quotes and trimming because of https://github.com/scala/bug/issues/6476
  val emptyInput                 = new CalcCompilationException("cannot run computation on empty input")
  def invalidElem(elem: String)  = new CalcCompilationException(s""" "$elem" is not a number or one of the following operators ${ops.mkString(", ")}""".trim)
  def missingLeftInput(op: Op)   = new CalcCompilationException(s"cannot start input with an operator: started with $op")
  def invalidSeq(seq: String)    = new CalcCompilationException(s""" "$seq" is not a valid sequence""".trim)
  def missingRightInput(op: Op)  = new CalcCompilationException(s"""operator "$op" missing right-hand input""")
  val divByZero                  = new CalcRuntimeException("cannot divide by zero")
  val boom                       = new CalcCompilationException("unknown compilation error")

  def run(input: String): Try[Double] = for {
    elems  <- lex(input)
    valid  <- parse(elems)
    result <- eval(valid)
  } yield result

  private[calc] def lex(input: String): Try[List[Tok]] =
    input.split(' ').toList.map {
      case ""  => Failure(emptyInput)
      case "+" => Success(TOp(Add))
      case "-" => Success(TOp(Sub))
      case "*" => Success(TOp(Mult))
      case "/" => Success(TOp(Div))
      case num => Try(num.toDouble)
        .fold(_ => Failure(invalidElem(num)), double => Success(TNum(double)))
    }
      .sequence

  // uses shunting-yard algorithm to build a parse tree. Could build BNF instead, but this is more generic.
  // https://en.wikipedia.org/wiki/Shunting-yard_algorithm
  private[calc] def parse(input: List[Tok]): Try[ParseTree] = {
    def go(input: List[Tok], parseTree: List[ParseTree], shuntStack: List[TOp]): Try[ParseTree] = (input, parseTree, shuntStack) match {
      case (Nil, res ::  Nil, Nil) =>
        Success(res)

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

      // TODO add cases for explicit errors
      case _ =>
        Failure(boom)
    }

    go(input, List.empty, List.empty)
  }

  def eval(parsed: ParseTree): Try[Double] = {
    //TODO deal with results that are outside the double representation
    def runOp(op: Op, args: (Double, Double)): Try[Double] = (op, args) match {
      case (Add,  (l, r))            => Success(l + r)
      case (Sub,  (l, r))            => Success(l - r)
      case (Mult, (l, r))            => Success(l * r)
      case (Div,  (l, r)) if r != 0  => Success(l / r)
      case (Div,  (_, _))            => Failure(divByZero)
    }

    parsed match {
      case PNum(answer) =>
        Success(answer)

      case POp(op, param0, param1) =>
        for {
          p0  <- eval(param0)
          p1  <- eval(param1)
          ans <- runOp(op, (p0, p1))
        } yield ans
    }
  }
}
