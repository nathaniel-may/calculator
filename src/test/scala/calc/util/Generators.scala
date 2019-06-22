package calc.util

// Scalacheck
import org.scalacheck.Gen, Gen.posNum
import Gen.nonEmptyListOf
import org.scalacheck.Arbitrary.{arbDouble, arbLong, arbInt, arbBool}

// Scala
import scala.annotation.tailrec

// Project
import calc.Language, Language.{Tok, TNum, TOp}

object Generators {

  val whitespaceGen:    Gen[String] = Gen.oneOf(" \t\n".toList).map(_.toString)
  val whitespaceStrGen: Gen[String] = Gen.listOf(whitespaceGen).map(_.mkString(""))
  val doubleGen:        Gen[Double] = arbDouble.arbitrary
  val longGen:          Gen[Long]   = arbLong.arbitrary
  val opGen:            Gen[TOp]    = Gen.oneOf(Language.ops).map(TOp(_))
  val numOpGen:         Gen[Char]   = Gen.numChar.flatMap { num =>
      Gen.oneOf(num :: Language.ops.map(_.toString.charAt(0))) }

  val numGen: Gen[TNum] = for {
    int     <- posNum[Int] // for now to not confuse with minus
    point   <- arbBool.arbitrary
    decimal <- posNum[Int]
  } yield TNum(BigDecimal(if(point) s"$int.$decimal" else s"$int"))

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
    case TNum(num)   => num.toString
    case TOp(op) => op.toString
  } mkString " "

}
