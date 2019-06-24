package calc

import calc.Lexer.{TNum, TOp, Tok}
import calc.Parse.{ParseTree, POp, PNum}
import calc.Exceptions.{EmptyInputErr, InvalidSequenceErr, MissingLeftInputErr, MissingRightInputErr, UnknownCompilationErr}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

// uses shunting-yard algorithm to build a parse tree. Could build BNF instead, but this is more generic.
// https://en.wikipedia.org/wiki/Shunting-yard_algorithm
private[calc] object Parser {

  def run(input: List[Tok]): Try[ParseTree] = {
    @tailrec
    def go(input: List[Tok], parseTree: List[ParseTree], shuntStack: List[TOp]): Try[ParseTree] = (input, parseTree, shuntStack) match {
      case (Nil, res ::  Nil, Nil) =>
        Success(res)

      case (Nil, Nil, Nil) =>
        Failure(new EmptyInputErr)

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

      case (TOp(op) :: tail, tree, stack @ TOp(shunted) :: _)
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

}
