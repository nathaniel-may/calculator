package calc

// Scalacheck
import org.scalacheck.Properties
import org.scalacheck.Prop.forAllNoShrink

// Project
import calc.Exceptions.CalcCompilationException
import calc.Lexer.Tok
import calc.util.Generators.seqGen

object EvaluatorProperties extends Properties("Evaluator"){

  property("runs without compilation errors") = forAllNoShrink(seqGen) {
    seq: List[Tok] =>
      Parser.run(seq)
        .flatMap(Evaluator.run)
        .fold(
          {
            case _: CalcCompilationException => false
            case _ => true
          },
          _ => true)
  }

}
