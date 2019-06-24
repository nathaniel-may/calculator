package calc.util

import org.scalacheck.Arbitrary

import calc.Lexer.{Tok, TNum, TOp}
import calc.util.Generators.{tokGen, numGen, opGen}

object Arbs {

  implicit val arbNum:  Arbitrary[TOp]  = Arbitrary(opGen)
  implicit val arbOp:   Arbitrary[TNum] = Arbitrary(numGen)
  implicit val arbTok:  Arbitrary[Tok]  = Arbitrary(tokGen)

}
