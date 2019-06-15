package calc

import org.scalatest._


class MainUnitTest extends FlatSpec with Matchers {

  "The main entry point" should "not throw exceptions for valid input" in {
    noException should be thrownBy Main.run(List("1 + 2 + 3 - 6")).unsafeRunSync()
  }

  it should "fail with no args" in {
    assertThrows[Exception] {
      Main.run(List())
        .unsafeRunSync()
    }
  }

  it should "fail with too many args" in {
    assertThrows[Exception] {
      Main.run(List("", ""))
        .unsafeRunSync()
    }
  }

}
