package calc

// Scalacheck
import org.scalacheck.Prop.{forAll, forAllNoShrink}
import org.scalacheck.Properties

// Project
import calc.util.Arbs._
import calc.util.Generators._
import calc.Exceptions._
import calc.Language.TNum


object CalculatorProperties extends Properties("calculator") {

  property("run evaluates valid input without compilation errors") = forAllNoShrink(inputGen) {
    input: String => Calculator.run(input).fold({
      case _: CalcCompilationException => false
      case _                           => true },
      _ => true)
  }

  property("run throws a runtime exception when dividing by zero") = forAll {
    num: TNum => Calculator.run(s"$num / 0").fold({
      case _: CalcRuntimeException => true
      case _                       => false },
      _ => false )
  }

}

