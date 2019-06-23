package calc

import scala.math.BigDecimal
import scala.util.matching.Regex
import scala.util.Try

object Language {

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
    val regex: Regex = raw"~?\d+\.?\d*".r // TODO: Match .5

    def from(str: String): Option[TNum] =
      Try(TNum(BigDecimal(
        if(str.startsWith("~")) s"-${str.drop(1)}"
        else str
      ))).toOption
  }

  case class TOp(value: Op) extends Tok {
    override def toString: String = value.toString
  }

  object TOp {
    val regex: Regex = raw"[-+*/]".r

    def from(str: String): Option[TOp] = str match {
      case "+" => Some(TOp(Add))
      case "-" => Some(TOp(Sub))
      case "*" => Some(TOp(Mult))
      case "/" => Some(TOp(Div))
      case _   => None
    }
  }

}
