package calc

import org.scalatest._


class MainUnitTest extends FlatSpec with Matchers {

  "The main entry point" should "not throw exceptions for valid input" in {
    noException should be thrownBy Main.run(List("1 + 2 + 3 - 6")).unsafeRunSync()
  }

  it should "not throw loudly to the user" in {
    noException should be thrownBy Calculator.run("1 + + 2")
    noException should be thrownBy Calculator.run("+")
    noException should be thrownBy Calculator.run("5 +")
    noException should be thrownBy Calculator.run("hello world")
  }

}