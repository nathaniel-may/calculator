package calc

import org.scalacheck.Prop.{forAll, forAllNoShrink}
import org.scalacheck.{Arbitrary, Gen, Properties}
import Gen.nonEmptyListOf
import org.scalacheck.Arbitrary.{arbDouble, arbLong}

import scala.util.Random
import shuffle.FunctionalShuffle.shuffle
import util.Generators._
import calc.Calculator.{CalcCompilationException, Tok, Number, Operation}


object CalculatorProperties extends Properties("calculator") {
  implicit val arbOp:  Arbitrary[Operation] = Arbitrary(opGen)
  implicit val arbTok: Arbitrary[Tok]       = Arbitrary(tokGen)
  val doubleGen = arbDouble.arbitrary
  val longGen   = arbLong.arbitrary

  property("lexer fails on bad inputs") = forAll {
    s: String => Calculator.lex(s)
      .fold(_ => true, _ => false)
  }

  property("lexer allows all doubles and operators in any order") =
    forAll(nonEmptyListOf(implicitly[Arbitrary[Tok]].arbitrary)) {
      elems: List[Tok] =>
        Calculator.lex(elems.map {
          case Operation(op) => op.toString
          case Number(num) => num.toString
        }
          .mkString(" "))
          .fold(_ => false, _ => true)
    }

  //TODO: failing on pattern like: 5 + 106 / 5 - 1
  property("parser allows all inputs in the form { num (op num)* }") =
    forAllNoShrink(seqGen) {
      seq: List[Tok] =>
        Calculator.parse(seq)
          .fold(_ => false, _ => true)
    }

  property("parser does not allow inputs that start with an operator or empty inputs") =
    forAllNoShrink(seqGen) {
      seq: List[Tok] =>
        Calculator.parse(seq.tail)
          .fold(_ => true, _ => false)
    }

  property("parser does not allow inputs with two numbers in a row") =
    forAllNoShrink(seqGen, numberGen, longGen) {
      (seq: List[Tok], num: Number, seed: Long) => (for {
        badSeq <- shuffle(num :: seq toStream)
      } yield Calculator.parse(badSeq.toList)
        .fold(_ => true, _ => false))
        .eval(new Random(seed))
    }

  property("parser does not allow inputs with two operators in a row") =
    forAllNoShrink(seqGen, opGen, longGen) {
      (seq: List[Tok], op: Operation, seed: Long) => (for {
        badSeq <- shuffle(op :: seq toStream)
      } yield Calculator.parse(badSeq.toList)
        .fold(_ => true, _ => false))
        .eval(new Random(seed))
    }

  property("evaluator runs without compilation errors") = forAllNoShrink(seqGen) {
    seq: List[Tok] =>
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

