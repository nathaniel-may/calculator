package util

import org.scalacheck.Gen, Gen.nonEmptyListOf
import org.scalacheck.Arbitrary.arbDouble
import scala.annotation.tailrec
import calc.Calculator, Calculator.Op

object Generators {

  val opGen: Gen[Op] = Gen.oneOf(Calculator.ops)

  val seqGen: Gen[List[Either[Double, Op]]] = {
    // this impl always ends with the last elem of x regardless of how long y is
    def interleave[A, B](x: List[A], y: List[B]): List[Either[A, B]] = {
      type AB = Either[A, B]

      @tailrec
      def go(xx: List[AB], yy: List[AB], result: List[AB]): List[AB] = xx match {
        case Nil    => result
        case h :: t => go(yy, t, h :: result)
      }

      (go(x.map(Left(_)), y.map(Right(_)), List()) match {
        case unchanged @  Nil              => unchanged
        case unchanged @ (Left(_) :: _)    => unchanged
        case              Right(_) :: tail => tail
      }).reverse
    }

    for {
      nums <- nonEmptyListOf(arbDouble.arbitrary)
      ops  <- nonEmptyListOf(opGen)
    } yield interleave(nums, ops)
  }

  val inputGen: Gen[String] = for {
    seq <- seqGen
  } yield seq.map {
    case Left(num) => num.toString
    case Right(op) => op.toString
  } mkString " "

}
