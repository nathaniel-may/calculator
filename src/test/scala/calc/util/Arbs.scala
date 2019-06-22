package calc.util

import org.scalacheck.Arbitrary

import calc.Language.{TOp, Tok}
import calc.util.Generators.{opGen, tokGen}

object Arbs {

  implicit val arbOp:  Arbitrary[TOp] = Arbitrary(opGen)
  implicit val arbTok: Arbitrary[Tok] = Arbitrary(tokGen)

}
