package calc

import org.scalacheck.Prop.{forAll, forAllNoShrink}
import org.scalacheck.{Arbitrary, Gen, Properties}
import Gen.nonEmptyListOf
import org.scalacheck.Arbitrary.{arbDouble, arbLong}
import scala.util.Random
import shuffle.FunctionalShuffle.{shuffle, Rand}
import scalaz.Monad
import util.Generators._
import calc.Calculator.{Tok, TNum, TOp}
import calc.Exceptions._


object CalculatorProperties extends Properties("calculator") {
  implicit val arbOp:  Arbitrary[TOp] = Arbitrary(opGen)
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
          case TOp(op) => op.toString
          case TNum(num) => num.toString
        }
          .mkString(" "))
          .fold(_ => false, _ => true)
    }
  
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
      (seq: List[Tok], num: TNum, seed: Long) => (for {
        badSeq <- shuffle(num :: seq toStream)
      } yield Calculator.parse(badSeq.toList)
        .fold(_ => true, _ => false))
        .eval(new Random(seed))
    }

  property("parser does not allow inputs with two operators in a row") =
    forAllNoShrink(seqGen, opGen, longGen) {
      (seq: List[Tok], op: TOp, seed: Long) => (seq match {
        case n :: Nil       => Monad[Rand].point(n :: op :: op :: n :: Nil toStream)
        case n :: o :: tail => shuffle(o :: op :: tail.init toStream).map(n #:: _ #::: seq.reverse.head #:: Stream.empty) : Rand[Stream[Tok]]
        case _              => Monad[Rand].point(op :: op :: Nil toStream)
      }).map { stream => Calculator.parse(stream.toList).fold(_ => true, _ => false ) }
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
    input: String => Calculator.run(input).fold({
      case _: CalcCompilationException => false
      case _                           => true },
      _ => true)
  }

  property("run throws a runtime exception when dividing by zero") = forAllNoShrink {
    d: Double => Calculator.run(s"$d / 0").fold({
      case _: CalcRuntimeException => true
      case _                       => false },
      _ => false )
  }
}

