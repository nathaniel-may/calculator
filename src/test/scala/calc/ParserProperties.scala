package calc

// Scalacheck
import org.scalacheck.Prop.forAllNoShrink
import org.scalacheck.Properties

// Scala
import scala.util.Random
import scalaz.Monad
import shuffle.FunctionalShuffle.{Rand, shuffle}

// Project
import calc.Language.{Tok, TNum, TOp}
import calc.Exceptions.CalcCompilationException
import calc.util.Generators.{longGen, numberGen, opGen, seqGen}

class ParserProperties extends Properties("Parser") {

  property("parser allows all inputs in the form { num (op num)* }") =
    forAllNoShrink(seqGen) {
      seq: List[Tok] =>
        Parser.run(seq)
          .fold(_ => false, _ => true)
    }

  property("parser does not allow inputs that start with an operator or empty inputs") =
    forAllNoShrink(seqGen) {
      seq: List[Tok] =>
        Parser.run(seq.tail)
          .fold(_ => true, _ => false)
    }

  property("parser does not allow inputs with two numbers in a row") =
    forAllNoShrink(seqGen, numberGen, longGen) {
      (seq: List[Tok], num: TNum, seed: Long) => (for {
        badSeq <- shuffle(num :: seq toStream)
      } yield Parser.run(badSeq.toList)
        .fold(_ => true, _ => false))
        .eval(new Random(seed))
    }

  property("parser does not allow inputs with two operators in a row") =
    forAllNoShrink(seqGen, opGen, longGen) {
      (seq: List[Tok], op: TOp, seed: Long) => (seq match {
        case n :: Nil       => Monad[Rand].point(n :: op :: op :: n :: Nil toStream)
        case n :: o :: tail => shuffle(o :: op :: tail.init toStream).map(n #:: _ #::: seq.reverse.head #:: Stream.empty) : Rand[Stream[Tok]]
        case _              => Monad[Rand].point(op :: op :: Nil toStream)
      }).map { stream => Parser.run(stream.toList).fold(_ => true, _ => false ) }
        .eval(new Random(seed))
    }

}
