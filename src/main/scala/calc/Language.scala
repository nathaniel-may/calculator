package calc

import scala.math.BigDecimal

private[calc] object Language {

  object Priority extends Enumeration {
    type Priority = Value
    val Second, First = Value
  }

  sealed trait Op {
    val priority: Priority.Priority
  }

  object Mult extends Op {
    val priority = Priority.First
    override def toString: String = "*"
  }

  object Div  extends Op {
    val priority = Priority.First
    override def toString: String = "/"
  }

  object Add  extends Op {
    val priority = Priority.Second
    override def toString: String = "+"
  }

  object Sub  extends Op {
    val priority = Priority.Second
    override def toString: String = "-"
  }

  val ops = List(Add, Sub, Mult, Div)

  sealed trait Tok
  case class TNum(value: BigDecimal) extends Tok
  case class TOp(value: Op)          extends Tok

}
