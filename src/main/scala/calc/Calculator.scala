package calc

import scala.util.Try
import scala.math.BigDecimal

object Calculator {

  def run(input: String): Try[BigDecimal] =
    Lexer.run(input) flatMap Parser.run flatMap Evaluator.run

}
