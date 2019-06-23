package calc

import cats.Applicative
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import scopt.OptionParser
import scala.io.StdIn
import calc.Exceptions.ArgumentParsingErr


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

  private def calculate(input: String) =
    Calculator.run(input)
      .fold(Left(_), Right(_))

  private def printResult(res: Either[Throwable, BigDecimal]) =
    res match {
      case Right(ans) => (printlnIO(ans), ExitCode.Success)
      case Left(e)    => (printlnIO(e.getMessage), ExitCode.Error)
    }

  private def printlnIO(x: Any): IO[Unit] =
    IO(println(x))

  private def printIO(x: Any): IO[Unit] =
    IO(print(x))

  private def readLineIO: IO[String] =
    IO(StdIn.readLine())

  private def repl: IO[Unit] = {
    def go: IO[Unit] = for {
      _   <- printlnIO("")
      _   <- printIO("calc> ")
      cmd <- readLineIO
      _   <- Applicative[IO].unlessA(cmd.trim == "exit") {
               printResult(calculate(cmd))._1 *> go
             }
    } yield ()

    printlnIO(":::: This is the calc repl - type `exit` to quit ::::") *> go
  }

  override def run(args: List[String]): IO[ExitCode] =
    parser.parse(args, ReplConfig) match {
      case Some(EvalConfig(input)) =>
        val (io, exitCode) = printResult(calculate(input))
        io.as(exitCode)

      case Some(ReplConfig) =>
        repl.as(ExitCode.Success)

      case None =>
        IO.raiseError(new ArgumentParsingErr)
    }
}
