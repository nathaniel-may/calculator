package util

import org.scalacheck.Gen
import Gen.nonEmptyListOf
import org.scalacheck.Arbitrary.{arbDouble, arbLong}

import scala.annotation.tailrec
import calc.Calculator, Calculator.{Tok, TNum, TOp}

object Generators {

  val doubleGen: Gen[Double] = arbDouble.arbitrary
  val longGen:   Gen[Long]   = arbLong.arbitrary
  val opGen:     Gen[TOp]    = Gen.oneOf(Calculator.ops).map(TOp)
  val numberGen: Gen[TNum]   = arbDouble.arbitrary.map(d => TNum(BigDecimal(d)))
  val tokGen:    Gen[Tok]    = Gen.oneOf(opGen, numberGen)
  val numOpGen: Gen[Char]    = Gen.numChar.flatMap { num =>
      Gen.oneOf(num :: Calculator.ops.map(_.toString.charAt(0))) }

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
      nums <- nonEmptyListOf(numberGen)
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
