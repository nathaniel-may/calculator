package calc

import org.scalacheck.Prop.{forAll, forAllNoShrink}
import org.scalacheck.{Arbitrary, Gen, Properties}
import Gen.nonEmptyListOf
import org.scalacheck.Arbitrary.{arbDouble, arbLong}

import scala.util.Random
import shuffle.FunctionalShuffle.shuffle
import util.Generators._
import calc.Calculator.{CalcCompilationException, Op, EvalElem}


object CalculatorProperties extends Properties("calculator") {
  implicit val arbOp: Arbitrary[Op] = Arbitrary(opGen)
  val doubleGen = arbDouble.arbitrary
  val longGen   = arbLong.arbitrary

  property("lexer fails on bad inputs") = forAll {
    s: String => Calculator.lex(s)
      .fold(_ => true, _ => false)
  }

  property("lexer allows all doubles and operators in any order") =
    forAll(nonEmptyListOf(implicitly[Arbitrary[EvalElem]].arbitrary)) {
      elems: List[EvalElem] =>
        Calculator.lex(elems.map {
          case Right(op) => op.toString
          case Left(num) => num.toString
        }
          .mkString(" "))
          .fold(_ => false, _ => true)
    }

  //TODO: failing on pattern like: 5 + 106 / 5 - 1
  property("parser allows all inputs in the form { num (op num)* }") =
    forAllNoShrink(seqGen) {
      seq: List[EvalElem] =>
        Calculator.parse(seq)
          .fold(_ => false, _ => true)
    }

  property("parser does not allow inputs that start with an operator or empty inputs") =
    forAllNoShrink(seqGen) {
      seq: List[EvalElem] =>
        Calculator.parse(seq.tail)
          .fold(_ => true, _ => false)
    }

  property("parser does not allow inputs with two numbers in a row") =
    forAllNoShrink(seqGen, doubleGen, longGen) {
      (seq: List[EvalElem], num: Double, seed: Long) => (for {
        badSeq <- shuffle(Left(num) :: seq toStream)
      } yield Calculator.parse(badSeq.toList)
        .fold(_ => true, _ => false))
        .eval(new Random(seed))
    }

  property("parser does not allow inputs with two operators in a row") =
    forAllNoShrink(seqGen, opGen, longGen) {
      (seq: List[EvalElem], op: Op, seed: Long) => (for {
        badSeq <- shuffle(Right(op) :: seq toStream)
      } yield Calculator.parse(badSeq.toList)
        .fold(_ => true, _ => false))
        .eval(new Random(seed))
    }

  property("evaluator runs without compilation errors") = forAllNoShrink(seqGen) {
    seq: List[EvalElem] =>
      Calculator.parse(seq)
        .flatMap(Calculator.eval)
        .fold(
          {
            case _: CalcCompilationException => false
            case _ => true
          },
          _ => true)
  }

  property("run evaluates valid input without compilation errors") = forAllNoShrink(inputGen) {
    input: String => Calculator.run(input)
      .fold(
        {
          case _: CalcCompilationException => false
          case _ => true
        },
        _ => true)
  }
}

