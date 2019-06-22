package util

import org.scalacheck.Arbitrary

import calc.Calculator.{TOp, Tok}
import util.Generators.{opGen, tokGen}

object Arbs {

  implicit val arbOp:  Arbitrary[TOp] = Arbitrary(opGen)
  implicit val arbTok: Arbitrary[Tok] = Arbitrary(tokGen)

}
