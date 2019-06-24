package calc.util

// Scalacheck
import org.scalacheck.{Arbitrary, Gen}
import Gen.posNum
import Gen.nonEmptyListOf
import org.scalacheck.Arbitrary.{arbBool, arbChar, arbDouble, arbInt, arbLong, arbOption}

// Scala
import scala.annotation.tailrec

// Project
import calc.Lexer.{Tok, TNum, TOp}
import calc.Parse.ops

object Generators {

  val whitespaceGen:    Gen[String]  = Gen.oneOf(" \t\n".toList).map(_.toString)
  val whitespaceStrGen: Gen[String]  = Gen.listOf(whitespaceGen).map(_.mkString(""))
  val intGen:           Gen[Int]     = arbInt.arbitrary
  val doubleGen:        Gen[Double]  = arbDouble.arbitrary
  val longGen:          Gen[Long]    = arbLong.arbitrary
  val boolGen:          Gen[Boolean] = arbBool.arbitrary
  val opGen:            Gen[TOp]     = Gen.oneOf(ops).map(TOp(_))
  val numOpGen:         Gen[Char]    = Gen.numChar.flatMap { num =>
      Gen.oneOf(num :: ops.map(_.toString.charAt(0))) }

  val nonEmptyStrGen: Gen[String] = Gen.nonEmptyListOf[Char](arbChar.arbitrary).map(_.mkString)

  private def toNumString(neg: Boolean, int: Option[Int], point: Boolean, decimal: Option[Int]) = {
    val s = if (neg) "-" else "" +
      int.fold("")(_.toString) +
      (if (point) "." else "") +
      decimal.fold("")(_.toString)

    if (s.length <= 1) "0" else s
  }

  val numGen: Gen[TNum] = for {
    neg     <- boolGen
    int     <- arbOption(Arbitrary(posNum[Int])).arbitrary
    point   <- boolGen
    decimal <- arbOption(Arbitrary(posNum[Int])).arbitrary
  } yield TNum(BigDecimal(toNumString(neg, int, point, decimal)))

  val tokGen: Gen[Tok] = Gen.oneOf(opGen, numGen)

  val seqGen: Gen[List[Tok]] = {
    // this impl always ends with the last elem of x regardless of how long y is
    def interleave[A, B](x: List[A], y: List[B]): List[Either[A, B]] = {
      type AB = Either[A, B]

      @tailrec
      def go(xx: List[AB], yy: List[AB], result: List[AB]): List[AB] = xx match {
        case Nil    => result
        case h :: t => go(yy, t, h :: result)
      }

      (go(x.map(Left(_)), y.map(Right(_)), List()) match {
        case             Nil              => Nil
        case unchanged @ Left(_)  :: _    => unchanged
        case             Right(_) :: tail => tail
      }).reverse
    }

    for {
      nums <- nonEmptyListOf(numGen)
      ops  <- nonEmptyListOf(opGen)
    } yield interleave(nums, ops) map {
      case Left(num) => num
      case Right(op) => op
    }
  }

  val inputGen: Gen[String] = for {
    seq <- seqGen
  } yield seq.map {
    case num: TNum => num.toString
    case op:  TOp  => op.toString
  } mkString " "

}
