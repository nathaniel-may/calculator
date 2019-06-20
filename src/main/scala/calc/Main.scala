package calc

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import scopt.OptionParser
import calc.Exceptions.ArgumentParsingErr


object Main extends IOApp {

  case class Config(input: String = "")

  val parser: OptionParser[Config] =
    new OptionParser[Config]("calc") {
      head("calc", "")

      arg[String]("<compute string>")
        .required
        .action { (s, c) => c.copy(input = s) }
        .text("...four function string to compute")
    }

  def go(args: List[String]): IO[BigDecimal] = {
    parser.parse(args, Config()) match {
      case Some(config) =>
        Calculator.run(config.input)
          .fold(IO.raiseError, IO(_))

      case None =>
        IO.raiseError(new ArgumentParsingErr)
    }
  }

  override def run(args: List[String]): IO[ExitCode] =
    go(args)
      .handleErrorWith { e => IO(println(e.getMessage)).as(ExitCode.Error) }
      .flatMap { ans => IO(println(ans)).as(ExitCode.Success) }

}
