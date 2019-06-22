package calc

import calc.Language.{TNum, TOp, Tok}
import calc.Exceptions.{EmptyInputErr, InvalidElementErr}

import scala.util.{Failure, Success, Try}

private[calc] object Lexer {

  def handleTilde(str: String): String =
    if(str.startsWith("~")) s"-${str.drop(1)}"
    else str

  def run(input: String): Try[List[Tok]] = {
    def go(in: String, toks: List[Tok]): Try[List[Tok]] = {
      if (in.isEmpty && toks.isEmpty)
        Failure(new EmptyInputErr)
      else if (in.isEmpty)
        Success(toks.reverse)
      else {
        val prefix: Either[(Tok, String), _] = for {
          _ <- TNum.regex.findPrefixMatchOf(in)
                 .map { numStr => (TNum(BigDecimal(handleTilde(numStr.toString))), numStr.toString) }
                 .toLeft(())
          _ <- TOp.regex.findPrefixMatchOf(in)
                 .flatMap { opStr => TOp.fromString(opStr.toString).map((_, opStr.toString)) }
                 .toLeft(())
        } yield ()

        prefix.fold(
          { case (tok, str) => go(in.stripPrefix(str).trim, tok :: toks) },
          _ => Failure(InvalidElementErr.from(in.take(1)))
        )
      }

    }

    go(input.trim, List.empty)
  }

}
