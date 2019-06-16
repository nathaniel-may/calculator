package calc

import org.scalacheck.Prop.{forAll, forAllNoShrink}
import org.scalacheck.{Arbitrary, Gen, Properties}
import Gen.nonEmptyListOf
import org.scalacheck.Arbitrary.{arbDouble, arbLong}

import scala.util.Random
import shuffle.FunctionalShuffle.shuffle
import util.Generators._
import calc.Calculator.{CalcCompilationException, Op}


object CalculatorProperties extends Properties("calculator") {
  implicit val arbOp: Arbitrary[Op] = Arbitrary(opGen)
  val doubleGen = arbDouble.arbitrary
  val longGen   = arbLong.arbitrary

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

  //TODO: failing on pattern like: 5 + 106 / 5 - 1
  property("parser allows all inputs in the form { num (op num)* }") =
    forAllNoShrink(seqGen) {
      seq: List[CalcTree] =>
        Calculator.parse(seq)
          .fold(e => {println("^^^^^^^^"); println(e); printStackTrace(e); false }, _ => true)
    }

  //TODO this is bullshit
  def printStackTrace(e: Throwable) =
    e.getStackTrace.toList.map(println(_))

  property("parser does not allow inputs that start with an operator") =
    forAllNoShrink(seqGen) {
      seq: List[CalcTree] =>
        Calculator.parse(seq.tail)
          .fold(_ => true, _ => false)
    }

  property("parser does not allow inputs with two numbers in a row") =
    forAllNoShrink(seqGen, doubleGen, longGen) {
      (seq: List[CalcTree], num: Double, seed: Long) => (for {
        badSeq <- shuffle(Literal(num) :: seq toStream)
      } yield Calculator.parse(badSeq.toList)
        .fold(_ => true, _ => false))
        .eval(new Random(seed))
    }

  property("parser does not allow inputs with two operators in a row") =
    forAllNoShrink(seqGen, opGen, longGen) {
      (seq: List[CalcTree], op: Op, seed: Long) => (for {
        badSeq <- shuffle(Operator(op, (Empty, Empty)) :: seq toStream)
      } yield Calculator.parse(badSeq.toList)
        .fold(_ => true, _ => false))
        .eval(new Random(seed))
    }

  property("evaluator runs without compilation errors") = forAllNoShrink(seqGen) {
    seq: List[CalcTree] =>
      Calculator.parse(seq)
        .flatMap(Calculator.eval)
        .fold(
          {
            case _: CalcCompilationException => false
            case _ => true
          },
          _ => true)
  }

  property("calculator evaluates valid input without compilation errors") = forAllNoShrink(inputGen) {
    input: String => Calculator.run(input)
      .fold(
        {
          case _: CalcCompilationException => false
          case _ => true
        },
        _ => true)
  }
}

