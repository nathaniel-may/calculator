package calc

import scala.util.Try
import scala.math.BigDecimal

object Calculator {

  def run(input: String): Try[BigDecimal] = for {
    elems  <- Lexer.run(input)
    valid  <- Parser.run(elems)
    result <- Evaluator.run(valid)
  } yield result

}
