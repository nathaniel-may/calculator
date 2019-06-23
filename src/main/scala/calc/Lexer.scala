package calc

// Scala
import cats.syntax.all._
import cats.instances.all._
import cats.data.{NonEmptyList, StateT}
import scala.util.matching.Regex
import scala.util.Try

// Project
import calc.Language.{TNum, TOp, Tok}
import calc.Exceptions.InvalidElementErr
import calc.Implicits._
import calc.Instances._

private[calc] object Lexer {
  type RegexLexer[A] = (Regex, String => Try[A])

  def lex[A](rl: RegexLexer[A]): StateT[Try,String,A] = rl match {
    case (rx, build) =>
      StateT { s => for {
        matched <- rx.findPrefixMatchOf(s)
                     .toTry(InvalidElementErr.from(s.take(1)))
        built   <- build(matched.toString)
      } yield (s.drop(matched.end).trim, built) }
    }

  val tokens: NonEmptyList[RegexLexer[Tok]] = NonEmptyList.of(
    (TNum.regex, s => TNum.from(s).toTry(InvalidElementErr.from(s take 1))),
    (TOp.regex,  s => TOp.from(s).toTry(InvalidElementErr.from(s take 1)))
  )

  def run(input: String): Try[List[Tok]] = {
    def go: StateT[Try, String, List[Tok]] =
      StateT.inspect[Try, String, Boolean](_.isEmpty) flatMap {
        case true  => StateT.pure(List.empty)
        case false => for {
          tok  <- tokens.reduceMapK(lex)
          toks <- go
        } yield tok :: toks
      }
    go.runA(input.trim)
  }
}