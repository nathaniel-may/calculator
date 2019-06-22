package calc

import scala.math.BigDecimal
import scala.util.matching.Regex

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

  case class TNum(value: BigDecimal) extends Tok {
    override def toString: String = {
      val raw = value.toString
      if (raw.startsWith("-")) s"~${raw.drop(1)}"
      else raw
    }
  }

  object TNum {
    val regex: Regex = raw"~?\d+\.?\d*".r
  }

  case class TOp(value: Op) extends Tok {
    override def toString: String = value.toString
  }

  object TOp {
    val regex: Regex = raw"[+-\/*]".r

    def fromString(str: String): Option[TOp] = str.toList match {
      case '+' :: Nil => Some(TOp(Add))
      case '-' :: Nil => Some(TOp(Sub))
      case '*' :: Nil => Some(TOp(Mult))
      case '/' :: Nil => Some(TOp(Div))
      case _          => None
    }
  }

}
