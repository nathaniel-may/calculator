package calc

import cats.implicits._
import cats.implicits.catsStdInstancesForTry
import calc.Language.{Add, Div, Mult, Sub, TNum, TOp, Tok}
import calc.Exceptions.{EmptyInputErr, InvalidElementErr}

import scala.util.{Failure, Success, Try}

private[calc] object Lexer {

  def run(input: String): Try[List[Tok]] =
    input.split(' ').toList.map {
      case "" => Failure(new EmptyInputErr)
      case "+" => Success(TOp(Add))
      case "-" => Success(TOp(Sub))
      case "*" => Success(TOp(Mult))
      case "/" => Success(TOp(Div))
      case num => Try(num.toDouble)
        .fold(_ => Failure(InvalidElementErr.from(num)), double => Success(TNum(double)))
    } sequence
}
