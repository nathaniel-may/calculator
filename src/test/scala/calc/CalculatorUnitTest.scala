package calc

import org.scalatest._
import calc.Exceptions._

class CalculatorUnitTest extends FlatSpec with Matchers {
  val maxDouble: String = "179769313486231570000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

  "A Calculator" should "evaluate to the correct answer when order of operations doesn't matter" in {
    Calculator.run("1 + 2 + 3 - 6") fold(fail(_), _ shouldBe 0)
    Calculator.run("1 * 2 + 3 - 4") fold(fail(_), _ shouldBe 1)
    Calculator.run("2 / 2 + 3 - 2") fold(fail(_), _ shouldBe 2)
    Calculator.run("2 * 2 * 3 / 4") fold(fail(_), _ shouldBe 3)
  }

  it should "evaluate to the correct answer when order of operations does matter" in {
    Calculator.run("1 + 2 * 3 - 7") fold(fail(_), _ shouldBe 0)
    Calculator.run("1 - 3 + 6 / 2") fold(fail(_), _ shouldBe 1)
    Calculator.run("2 + 4 / 2 - 2") fold(fail(_), _ shouldBe 2)
    Calculator.run("2 * 3 - 1 * 3") fold(fail(_), _ shouldBe 3)
  }

  it should "handle numbers beyond the scale of Double" in {
    Calculator.run(s"$maxDouble * 2") fold(fail(_), _ > Double.MaxValue shouldBe true)
  }

  it should "throw the errors listed in the readme example" in {
    Calculator.run("1 + + 2")     fold(_ shouldBe a[InvalidSequenceErr],   _ => fail())
    Calculator.run("+")           fold(_ shouldBe a[MissingLeftInputErr],  _ => fail())
    Calculator.run("5 +")         fold(_ shouldBe a[MissingRightInputErr], _ => fail())
    Calculator.run("hello world") fold(_ shouldBe a[InvalidElementErr],    _ => fail())
  }
}
