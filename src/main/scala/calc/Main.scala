package calc

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import scopt.OptionParser
import scala.io.StdIn

import Repl.repl


object Main extends IOApp {

  sealed private trait Config
  private case class  EvalConfig(input: String) extends Config
  private case object ReplConfig                extends Config

  private val parser: OptionParser[Config] =
    new OptionParser[Config]("calc") {
      head("calc", "")

      arg[String]("<compute string>")
        .optional
        .action { (s, _) => EvalConfig(s) }
        .text("...four function string to compute")

      cmd("repl")
        .action { (_,_) => ReplConfig }
        .text("repl is a command.")
    }

  private[calc] def calculate(input: String) =
    Calculator.run(input)
      .fold(Left(_), Right(_))

  private[calc] def printResult(res: Either[Throwable, BigDecimal]) =
    res match {
      case Right(ans) => (printlnIO(ans), ExitCode.Success)
      case Left(e)    => (printlnIO(e.getMessage), ExitCode.Error)
    }

  private[calc] def printlnIO(x: Any): IO[Unit] =
    IO(println(x))

  private[calc] def printIO(x: Any): IO[Unit] =
    IO(print(x))

  private[calc] def readLineIO: IO[String] =
    IO(StdIn.readLine)

  override def run(args: List[String]): IO[ExitCode] =
    parser.parse(args, ReplConfig) match {
      case Some(EvalConfig(input)) =>
        val (io, exitCode) = printResult(calculate(input))
        io.as(exitCode)

      case Some(ReplConfig) =>
        repl.as(ExitCode.Success)

      case None =>
        IO(ExitCode.Error)
    }
}
