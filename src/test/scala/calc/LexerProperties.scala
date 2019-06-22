package calc

// Scalacheck
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}, Gen.nonEmptyListOf

// Project
import calc.util.Arbs._
import calc.Language.{Tok, TNum, TOp}

class LexerProperties extends Properties("Lexer") {

  property("lexer fails on bad inputs") = forAll {
    s: String => Lexer.run(s)
      .fold(_ => true, _ => false)
  }

  property("lexer allows all doubles and operators in any order") =
    forAll(nonEmptyListOf(implicitly[Arbitrary[Tok]].arbitrary)) {
      elems: List[Tok] =>
        Lexer.run(elems.map {
          case TOp(op)   => op.toString
          case TNum(num) => num.toString
        } mkString " " )
          .fold(_ => false, _ => true)
    }

}
