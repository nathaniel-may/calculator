package calc

// Scalacheck
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}, Gen.nonEmptyListOf

// Project
import calc.util.Arbs._
import calc.util.Generators._
import calc.Language.{Tok, TNum, TOp}

// TODO test for whitespace independence
class LexerProperties extends Properties("Lexer") {

  property("fails on bad inputs") = forAll {
    s: String => Lexer.run(s)
      .fold(_ => true, _ => false)
  }

  property("allows all numbers and operators in any order with any whitespace") =
    forAll(nonEmptyListOf(implicitly[Arbitrary[Tok]].arbitrary), whitespaceStrGen) {
      (elems: List[Tok], whitespace: String) =>
        Lexer.run(elems.map {
          case TOp(op)   => whitespace + op.toString
          case TNum(num) => num.toString + " " // "1.21.2" isn't valid input, but "1.2 1.2" is
        } mkString "")
          .fold(_ => false, _ => true)
    }

}
