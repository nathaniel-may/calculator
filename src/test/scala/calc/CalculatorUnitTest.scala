package calc

import org.scalatest._


class CalculatorUnitTest extends FlatSpec with Matchers {

  "A Calculator" should "evaluate to the correct answer when order of operations doesn't matter" in {
    Calculator.run("1 + 2 + 3 - 6").fold(fail(_), _ shouldBe 0)
    Calculator.run("1 * 2 + 3 - 4").fold(fail(_), _ shouldBe 1)
    Calculator.run("2 / 2 + 3 - 2").fold(fail(_), _ shouldBe 2)
  }

}
