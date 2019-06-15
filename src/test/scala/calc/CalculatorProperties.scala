package calc

import org.scalacheck.Prop.{forAll, forAllNoShrink}
import org.scalacheck.{Gen, Arbitrary, Properties}, Gen.nonEmptyListOf
import util.Generators._

import calc.Calculator.Op


object CalculatorProperties extends Properties("calculator") {
  implicit val arbOp: Arbitrary[Op] = Arbitrary(opGen)

  property("lexer fails on bad inputs") = forAll {
    s: String => Calculator.lex(s)
      .fold(_ => true, _ => false)
  }

  property("lexer allows all doubles and operators in any order") =
    forAll(nonEmptyListOf(implicitly[Arbitrary[Either[Double, Op]]].arbitrary)) {
      elems: List[Either[Double, Op]] =>
        Calculator.lex(elems.map {
          case Right(op) => op.toString
          case Left(num) => num.toString
        }
          .mkString(" "))
          .fold(_ => false, _ => true)
    }

  property("parser allows all inputs in the form { num (op num)* }") =
    forAllNoShrink(seqGen) {
      seq: List[Either[Double, Op]] =>
        Calculator.parse(seq)
          .fold(_ => false, _ => true)
    }
}

