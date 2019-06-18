package calc

import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec
import cats.implicits._

object Calculator {

  sealed trait Priority
  case object First  extends Priority
  case object Second extends Priority

  sealed trait Op {
    val priority: Priority
  }

  object Mult extends Op {
    val priority = First
    override def toString: String = "*"
  }

  object Div  extends Op {
    val priority = First
    override def toString: String = "/"
  }

  object Add  extends Op {
    val priority = Second
    override def toString: String = "+"
  }

  object Sub  extends Op {
    val priority = Second
    override def toString: String = "-"
  }

  val ops = List(Add, Sub, Mult, Div)

  sealed trait Tok
  case class Number(value: Double) extends Tok
  case class Operation(value: Op)  extends Tok

  class CalcCompilationException(message: String) extends Exception(message)
  class CalcRuntimeException(message: String)     extends Exception(message)

  // Some exception messages require triple quotes and trimming because of https://github.com/scala/bug/issues/6476
  val emptyInput                 = new CalcCompilationException("cannot run computation on empty input")
  def invalidElem(elem: String)  = new CalcCompilationException(s""" "$elem" is not a number or one of the following operators ${ops.mkString(", ")}""".trim)
  def missingLeftInput(op: Op)   = new CalcCompilationException(s"cannot start input with an operator: started with $op")
  def invalidSeq(seq: String)    = new CalcCompilationException(s""" "$seq" is not a valid sequence""".trim)
  def missingRightInput(op: Op)  = new CalcCompilationException(s"""operator "$op" missing right-hand input""")
  val divByZero                  = new CalcRuntimeException("cannot divide by zero")
  val boom                       = new CalcCompilationException("unknown compilation error")

  def run(input: String): Try[Double] = for {
    elems  <- lex(input)
    valid  <- parse(elems)
    result <- eval(valid)
  } yield result

  private[calc] def lex(input: String): Try[List[Tok]] =
    input.split(' ').toList.map {
      case ""  => Failure(emptyInput)
      case "+" => Success(Operation(Add))
      case "-" => Success(Operation(Sub))
      case "*" => Success(Operation(Mult))
      case "/" => Success(Operation(Div))
      case num => Try(num.toDouble)
        .fold(_ => Failure(invalidElem(num)), double => Success(Number(double)))
    }
      .sequence

  private[calc] def parse(input: List[Tok]): Try[List[Tok]] = {
      def validateStart(in: List[Tok]): Try[Unit] = in match {
        case Nil =>
          Failure(emptyInput)

        case Operation(op) :: _ =>
          Failure(missingLeftInput(op))

        case _ =>
          Success(())
      }

      @tailrec
      def go(in: List[Tok]): Try[Unit] = in match {
        case Nil =>
          Success(())

        case Operation(op0) :: Operation(op1) :: _ =>
          Failure(invalidSeq(s"$op0 $op1"))

        case Number(l0) :: Number(l1) :: _ =>
          Failure(invalidSeq(s"$l0 $l1"))

        case Operation(op) :: Nil =>
          Failure(missingRightInput(op))

        case Number(_) :: tail =>
          go(tail)

        case Operation(_) :: tail =>
          go(tail)
      }

      for {
        _ <- validateStart(input)
        _ <- go(input)
      } yield input
  }

  private[calc] def eval(parsed: List[Tok]): Try[Double] = {
    //TODO deal with results that are outside the double representation
    def runOp(op: Op, args: (Double, Double)): Try[Double] = {
      val math = (op, args) match {
        case (Add,  (l, r)) => l + r
        case (Sub,  (l, r)) => l - r
        case (Mult, (l, r)) => l * r
        case (Div,  (l, r)) => l / r
      }

      if (math == Double.NegativeInfinity || math == Double.PositiveInfinity)
        Failure(divByZero)
      else
        Success(math)
    }

    // Throws runtime exceptions TODO right choice? probs not.
    @tailrec
    def evalPass(ee: List[Tok], priority: Priority, out: List[Tok]): List[Tok] = ee match {
      case (l @ Number(_)) :: Nil =>
        (l :: out) reverse

      case Number(l) :: Operation(op) :: Number(r) :: tail if op.priority == priority =>
        evalPass((Number(runOp(op, (l, r)).get): Tok) :: tail, priority, out)

      case (l @ Number(_)) :: (op @ Operation(_)) :: (r @ Number(_)) :: tail =>
        evalPass(r :: tail, priority, op :: l :: out)

      // only unparsed inputs would reach here
      case _ =>
        throw boom
    }

    for {
      pass0 <- Try(evalPass(parsed, First,  List.empty))
      pass1 <- Try(evalPass(pass0,  Second, List.empty))
      res   <- pass1.headOption.fold[Try[Double]](Failure(boom)) {
                 case Number(res) => Success(res)
                 case _           => Failure(boom)
               }
    } yield res
  }
}
