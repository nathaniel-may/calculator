package calc

import calc.Calculator.CalcRuntimeException
import org.scalatest._


class CalculatorUnitTest extends FlatSpec with Matchers {

  "A Calculator" should "throw a runtime exception when dividing by zero" in {
    Calculator.run("1 / 0").fold(
      {
        case _: CalcRuntimeException => true
        case _                       => fail()
      },
      _ => fail()
    )

    Calculator.run("-1 / 0").fold(
      {
        case _: CalcRuntimeException => true
        case _                       => fail()
      },
      _ => fail()
    )
  }

  it should "evaluate to the correct answer when order of operations doesn't matter" in {
    Calculator.run("1 + 2 + 3 - 6").fold(fail(_), _ shouldBe 0)
    Calculator.run("1 * 2 + 3 - 4").fold(fail(_), _ shouldBe 1)
    Calculator.run("2 / 2 + 3 - 2").fold(fail(_), _ shouldBe 2)
    Calculator.run("2 * 2 * 3 / 4").fold(fail(_), _ shouldBe 3)
  }

  it should "evaluate to the correct answer when order of operations does matter" in {
    Calculator.run("1 + 2 * 3 - 7").fold(e => {println("***********"); println(e); printStackTrace(e); fail(e)}, _ shouldBe 0)
    Calculator.run("1 - 3 + 6 / 2").fold(e => {println("***********"); println(e); printStackTrace(e); fail(e)}, _ shouldBe 1)
    Calculator.run("2 + 4 / 2 - 2").fold(e => {println("***********"); println(e); printStackTrace(e); fail(e)}, _ shouldBe 2)
    Calculator.run("2 * 3 - 1 * 2").fold(e => {println("***********"); println(e); printStackTrace(e); fail(e)}, _ shouldBe 3)
  }

  def printStackTrace(e: Throwable) =
    e.getStackTrace.toList.filter(_.getFileName.contains("Calculator")).map(println(_))

}
