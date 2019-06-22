package calc

// Scalacheck
import org.scalacheck.Prop.{forAll, forAllNoShrink}
import org.scalacheck.Properties

// Project
import util.Generators._
import calc.Exceptions._


object CalculatorProperties extends Properties("calculator") {

  property("run evaluates valid input without compilation errors") = forAllNoShrink(inputGen) {
    input: String => Calculator.run(input).fold({
      case _: CalcCompilationException => false
      case _                           => true },
      _ => true)
  }

  property("run throws a runtime exception when dividing by zero") = forAll {
    d: Double => Calculator.run(s"$d / 0").fold({
      case _: CalcRuntimeException => true
      case _                       => false },
      _ => false )
  }

}

