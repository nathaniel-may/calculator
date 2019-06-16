package util

import calc.{CalcTree, Empty, Operator, Literal}

object ToString {

  def toString(tree: CalcTree): String = tree match {
    case Empty => "EMPTY"
    case Literal(l) => l.toString
    case Operator(op, (l, r)) => s"${toString(l)} ${op.toString} ${toString(r)}"
  }

}
