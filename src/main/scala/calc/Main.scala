package calc

import cats.effect.{IO, IOApp, ExitCode}
import cats.syntax.all._
import scopt.OptionParser


object Main extends IOApp {
  case class Config(input: String)

  val parser: OptionParser[Config] =
    new OptionParser[Config]("calc") {
      head("calc", "")

      arg[String]("compute string")
        .unbounded()
        .maxOccurs(1)
        .text("four function string to compute")
    }

  def run(args: List[String]): IO[ExitCode] = {
    parser.parse(args, Config("")) match {
      case Some(config) =>
        Calculator.run(config.input)
          .fold(
            e => IO(println(e.getMessage)).as(ExitCode.Success),
            output => IO(println(output)).as(ExitCode.Success))

      case None =>
        IO(()).as(ExitCode.Error)
    }
  }

}
