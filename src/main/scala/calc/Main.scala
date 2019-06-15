package calc

import cats.effect._
import cats.syntax.all._

object Main extends IOApp{
  val tooManyArgs = new Exception("Too many arguments. Expected a single string")
  val noArgs      = new Exception("Must pass a single string")

  override def run(args: List[String]): IO[ExitCode] = args match {
    case _ :: _ :: _  => IO.raiseError(tooManyArgs).as(ExitCode.Error)
    case Nil          => IO.raiseError(noArgs).as(ExitCode.Error)
    case input :: Nil =>
      Calculator
        .run(input)
        .fold(IO.raiseError(_).as(ExitCode.Success), IO(_).as(ExitCode.Success))
  }
}
