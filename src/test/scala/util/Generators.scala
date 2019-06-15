package util

import org.scalacheck.Gen
import calc.Calculator, Calculator.Op

object Generators {

  val opGen: Gen[Op] = Gen.oneOf(Calculator.ops)

}
