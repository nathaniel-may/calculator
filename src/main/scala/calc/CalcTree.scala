package calc

import Calculator.Op

sealed trait CalcTree
object Empty extends CalcTree
case class Literal(value: Double) extends CalcTree
case class Operator(value: Op, inputs: (CalcTree, CalcTree)) extends CalcTree
