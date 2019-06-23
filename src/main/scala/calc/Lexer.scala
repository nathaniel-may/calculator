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

  val tokens: NonEmptyList[RegexLexer[Tok]] = NonEmptyList.of(
    (TNum.regex, s => TNum.from(s).toTry(InvalidElementErr.from(s take 1))),
    (TOp.regex,  s => TOp.from(s).toTry(InvalidElementErr.from(s take 1)))
  )

  def lex[A](rl: RegexLexer[A]): StateT[Try, String, A] = {
    import StateT._
    val (rx, build) =  rl
    for {
      matched <- inspectF((s: String) => rx.findPrefixMatchOf(s)
                   .toTry(InvalidElementErr.from(s.take(1))))
      _       <- modify[Try, String](_.drop(matched.end).trim)
      built   <- liftF(build(matched.toString))
    } yield built
  }

  def run(input: String): Try[List[Tok]] =
    tokens.reduceMapK(lex)
      .untilM[List](StateT.inspect(_.isEmpty))
      .runA(input.trim)

}