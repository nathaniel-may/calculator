package calc

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import scopt.OptionParser
import scala.io.StdIn
import calc.Exceptions.ArgumentParsingErr


object Main extends IOApp {

  private case class Config(input: String = "", repl: Boolean = false)

  private val parser: OptionParser[Config] =
    new OptionParser[Config]("calc") {
      head("calc", "")

      arg[String]("<compute string>")
        .optional
        .action { (s, c) => c.copy(input = s) }
        .text("...four function string to compute")

      cmd("repl")
        .action { (_, c) => c.copy(repl = true) }
        .text("repl is a command.")
    }

  private def calculate(input: String) =
    Calculator.run(input)
      .fold(Left(_), Right(_))

  private def printResult(res: Either[Throwable, BigDecimal]) =
    res match {
      case Right(ans) => (printlnIO(ans), ExitCode.Error)
      case Left(e)    => (printlnIO(e.getMessage), ExitCode.Success)
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
      _   <- printIO("calc > ")
      cmd <- readLineIO
      _   <- if (cmd.trim == "exit") IO(())
             else printResult(calculate(cmd))._1
               .flatMap { _ => go }
    } yield ()

    printlnIO(":::: This is the calc repl - type `exit` to quit ::::")
      .flatMap { _ => go }
  }

  override def run(args: List[String]): IO[ExitCode] =
    parser.parse(args, Config()) match {
      case Some(Config(input, false)) =>
        val (io, exitCode) = printResult(calculate(input))
        io.as(exitCode)

      case Some(Config(_, true)) =>
        repl.map { _ => ExitCode.Success }

      case None =>
        IO.raiseError(new ArgumentParsingErr)
    }
}
