package calc

import calc.Parse.{Add, Div, Mult, Op, Sub, ParseTree, PNum, POp}
import calc.Exceptions.DivideByZeroErr

import scala.math.BigDecimal
import scala.util.{Failure, Success, Try}

private[calc] object Evaluator {

  def run(parsed: ParseTree): Try[BigDecimal] = {
    def runOp(op: Op, args: (BigDecimal, BigDecimal)): Try[BigDecimal] = (op, args) match {
      case (Add,  (l, r))            => Success(l + r)
      case (Sub,  (l, r))            => Success(l - r)
      case (Mult, (l, r))            => Success(l * r)
      case (Div,  (l, r)) if r != 0  => Success(l / r)
      case (Div,  (_, _))            => Failure(new DivideByZeroErr)
    }

    // stack overflow possible.
    // Scala impl on JVM makes it nearly impossible to catch safely.
    parsed match {
      case PNum(answer) =>
        Success(answer)

      case POp(op, param0, param1) =>
        for {
          p0  <- run(param0)
          p1  <- run(param1)
          ans <- runOp(op, (p0, p1))
        } yield ans
    }
  }

}
