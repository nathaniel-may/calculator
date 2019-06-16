package calc

import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec
import cats.implicits._

object Calculator {
  sealed trait OOO
  sealed trait Order0Op extends OOO
  sealed trait Order1Op extends OOO
  sealed trait Op
  object Add  extends Op with Order1Op { override def toString: String = "+" }
  object Sub  extends Op with Order1Op { override def toString: String = "-" }
  object Mult extends Op with Order0Op { override def toString: String = "*" }
  object Div  extends Op with Order0Op { override def toString: String = "/" }
  val ops = List(Add, Sub, Mult, Div)

  type EvalElem = Either[Double, Op]

  class CalcCompilationException(message: String) extends Exception(message)
  class CalcRuntimeException(message: String)     extends Exception(message)

  val boom                       = new CalcCompilationException("unknown compilation error") // TODO replace this with something more sane
  val emptyInput                 = new CalcCompilationException("cannot run computation on empty input")
  def invalidElem(elem: String)  = new CalcCompilationException(s"$elem is not a number or one of the following operators ${ops.mkString(", ")}")
  def missingLeftInput(op: Op)   = new CalcCompilationException(s"cannot start input with an operator: started with $op")
  def invalidSeq(seq: String)    = new CalcCompilationException(s"$seq is not a valid sequence")
  val emptyTree                  = new CalcCompilationException("cannot run computation on empty input")
  def missingRightInput(op: Op)  = new CalcCompilationException(s"operator $op missing right-hand input")
  val divByZero                  = new CalcRuntimeException("cannot divide by zero")

  def run(input: String): Try[Double] = for {
    elems  <- lex(input)
    valid  <- parse(elems)
    result <- eval(valid)
  } yield result

  private[calc] def lex(input: String): Try[List[EvalElem]] =
    input.split(' ').toList.map {
      case ""  => Failure(emptyInput)
      case "+" => Success(Right(Add))
      case "-" => Success(Right(Sub))
      case "*" => Success(Right(Mult))
      case "/" => Success(Right(Div))
      case num => Try(num.toDouble)
        .fold(_ => Failure(invalidElem(num)), double => Success(Left(double)))
    }
      .sequence

  private[calc] def parse(input: List[EvalElem]): Try[List[EvalElem]] = {

    def validate(in: List[EvalElem]): Try[Unit] = {
      def validateStart(in: List[EvalElem]): Try[Unit] = in match {
        case Right(op) :: _ =>
          Failure(missingLeftInput(op))

        case _ =>
          Success(())
      }

      @tailrec
      def go(in: List[EvalElem], err: Try[Unit]): Try[Unit] = in match {
        case Nil =>
          err

        case Right(op0) :: Right(op1) :: _ =>
          Failure(invalidSeq(s"$op0 $op1"))

        case Left(l0) :: Left(l1) :: _ =>
          Failure(invalidSeq(s"$l0 $l1"))

        case Right(op) :: Nil =>
          Failure(missingRightInput(op))

        case Left(_) :: tail =>
          go(tail, err)

        case Right(_) :: tail =>
          go(tail, err)
      }

      for {
        _ <- validateStart(in)
        _ <- go(in, Success(()))
      } yield ()
    }

    for {
      _ <- validate(input)
    } yield input
  }

  private[calc] def eval(parsed: List[EvalElem]): Try[Double] = {
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
    def evalPass0(ee: List[EvalElem], out: List[EvalElem]): List[EvalElem] = ee match {
      case Left(_) :: Nil =>
        out

      case Left(l) :: Right(op: Order0Op) :: Left(r) :: tail =>
        evalPass0((Left(runOp(op, (l, r)).get): EvalElem) :: tail, out)

      case (l @ Left(_)) :: (op @ Right(_)) :: (r @ Left(_)) :: tail =>
        evalPass0(r :: tail, l :: op :: out)

      case _ =>
        throw boom
    }

    // Throws runtime exceptions TODO right choice? probs not.
    @tailrec
    def evalPass1(ee: List[EvalElem], out: List[EvalElem]): Double = ee match {
      case Left(result) :: Nil =>
        result

      case Left(l) :: Right(op) :: Left(r) :: tail =>
        evalPass1((Left(runOp(op, (l, r)).get): EvalElem) :: tail, out)

      case _ =>
        throw boom
    }

    for {
      pass0 <- Try(evalPass0(parsed, List.empty))
      res   <- Try(evalPass1(pass0,  List.empty))
    } yield res
  }
}
