package calc

import scala.math.BigDecimal
import Calculator.Op

trait ParseTree
case class PNum(value: BigDecimal) extends ParseTree
case class POp(value: Op, param0: ParseTree, param1: ParseTree) extends ParseTree