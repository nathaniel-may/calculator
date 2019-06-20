package calc

import Calculator.Op

trait ParseTree
case class PNum(value: Double) extends ParseTree
case class POp(value: Op, param0: ParseTree, param1: ParseTree) extends ParseTree