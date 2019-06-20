package calc

import org.scalatest._


class CalculatorUnitTest extends FlatSpec with Matchers {

  "A Calculator" should "evaluate to the correct answer when order of operations doesn't matter" in {
    Calculator.run("1 + 2 + 3 - 6").fold(fail(_), _ shouldBe 0)
    Calculator.run("1 * 2 + 3 - 4").fold(fail(_), _ shouldBe 1)
    Calculator.run("2 / 2 + 3 - 2").fold(fail(_), _ shouldBe 2)
    Calculator.run("2 * 2 * 3 / 4").fold(fail(_), _ shouldBe 3)
  }

  it should "evaluate to the correct answer when order of operations does matter" in {
    Calculator.run("1 + 2 * 3 - 7").fold(fail(_), _ shouldBe 0)
    Calculator.run("1 - 3 + 6 / 2").fold(fail(_), _ shouldBe 1)
    Calculator.run("2 + 4 / 2 - 2").fold(fail(_), _ shouldBe 2)
    Calculator.run("2 * 3 - 1 * 3").fold(fail(_), _ shouldBe 3)
  }

  it should "throw the errors listed in the readme example" in {
    Calculator.run("1 + + 2").fold(_.getMessage should include ("is not a valid sequence"), _ => fail())
    Calculator.run("+").fold(_.getMessage should include ("cannot start input with an operator: started with"), _ => fail())
    Calculator.run("5 +").fold(_.getMessage should include ("missing right-hand input"), _ => fail())
    Calculator.run("hello world").fold(_.getMessage should include ("is not a number or one of the following operators"), _ => fail())
  }
}
