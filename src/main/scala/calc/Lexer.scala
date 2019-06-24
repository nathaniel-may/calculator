package calc

// Scala
import cats.syntax.all._
import cats.instances.all._
import cats.data.{NonEmptyList, StateT}

import scala.math.BigDecimal
import scala.util.matching.Regex
import scala.util.Try

// Project
import calc.Parse.{Add, Div, Mult, Op, Sub}
import calc.Exceptions.InvalidElementErr
import calc.Implicits._
import calc.Instances._

private[calc] object Lexer {

  sealed trait Tok

  case class TNum(value: BigDecimal) extends Tok {
    override def toString: String = {
      val raw = value.toString
      if (raw.startsWith("-")) s"~${raw.drop(1)}"
      else raw
    }
  }

  object TNum {
    val regex: Regex = raw"~?\d*\.?\d*".r

    def from(str: String): Option[TNum] =
      Try(TNum(BigDecimal(
        if(str.startsWith("~")) s"-${str.drop(1)}"
        else str
      ))).toOption
  }

  case class TOp(value: Op) extends Tok {
    override def toString: String = value.toString
  }

  object TOp {
    val regex: Regex = raw"[-+*/]".r

    def from(str: String): Option[TOp] = str match {
      case "+" => Some(TOp(Add))
      case "-" => Some(TOp(Sub))
      case "*" => Some(TOp(Mult))
      case "/" => Some(TOp(Div))
      case _   => None
    }
  }

  type RegexLexer[A] = (Regex, String => Try[A])

  val tokens: NonEmptyList[RegexLexer[Tok]] = NonEmptyList.of(
    (TNum.regex, s => TNum.from(s) toTry InvalidElementErr.from(s take 1)),
    (TOp.regex,  s => TOp.from(s) toTry InvalidElementErr.from(s take 1))
  )

  def lex[A](rl: RegexLexer[A]): StateT[Try, String, A] = {
    import StateT._
    val (rx, build) = rl
    for {
      matched <- inspectF((s: String) => rx.findPrefixMatchOf(s)
                   .toTry(InvalidElementErr.from(s take 1)))
      _       <- modify[Try, String](_.drop(matched.end).trim)
      built   <- liftF(build(matched.toString))
    } yield built
  }

  def run(input: String): Try[List[Tok]] =
    tokens.reduceMapK(lex)
      .untilM[List](StateT.inspect(_.isEmpty))
      .runA(input.trim)

}