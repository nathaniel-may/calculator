package calc

import scala.util.Try
import Calculator.Op

trait CalcTree
object CalcTree {
  def validate(tree: CalcTree): Try[CalcTree] = ???
}

object Empty extends CalcTree
case class Literal(value: Double) extends CalcTree
case class Operator(value: Op, inputs: (CalcTree, CalcTree)) extends CalcTree
