package calc

import scala.math.BigDecimal

object Parse {

  sealed trait ParseTree
  case class PNum(value: BigDecimal) extends ParseTree
  case class POp(value: Op, param0: ParseTree, param1: ParseTree) extends ParseTree

  object Priority extends Enumeration {
    type Priority = Value
    val Second, First = Value
  }

  sealed trait Paren {
    def from(s: String): Option[Paren] = s match {
      case "(" => Some(LParen)
      case ")" => Some(RParen)
      case _   => None
    }

    override def toString: String = this match {
      case LParen => "("
      case RParen => ")"
    }
  }

  object LParen extends Paren
  object RParen extends Paren

  sealed trait Op {
    val priority: Priority.Priority
  }

  object Mult extends Op {
    val priority = Priority.First

    override def toString: String = "*"
  }

  object Div extends Op {
    val priority = Priority.First

    override def toString: String = "/"
  }

  object Add extends Op {
    val priority = Priority.Second

    override def toString: String = "+"
  }

  object Sub extends Op {
    val priority = Priority.Second

    override def toString: String = "-"
  }

  val ops = List(Add, Sub, Mult, Div)
}