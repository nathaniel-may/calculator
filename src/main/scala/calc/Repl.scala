package calc

import calc.Main.{calculate, printIO, printResult, printlnIO, readLineIO}
import cats.data.{NonEmptyList, StateT}
import cats.effect.IO
import cats.syntax.all._

object Repl {
  type RunCMD = StateT[IO, String, Unit]
  val header: String = ":::: This is the calc repl - type `exit` to quit ::::"

  val repl: IO[Unit] =
    printlnIO(header) *> go

  val commands: NonEmptyList[(String => Boolean, String => IO[Unit])] = NonEmptyList(
    (_ == null,        _   => printlnIO("EOF")), List(
    (_.trim == "exit", _   => printIO("EXIT") >> IO(())),
    (_.trim == ":q",   _   => printIO("QUIT") >> IO(())),
    (_ => true,        cmd => printIO("CALC") >> printResult(calculate(cmd))._1 >> go))
  )

  def runCmd(in: (String => Boolean, String => IO[Unit])): RunCMD = {
    val (check, run) = in
    StateT.inspectF(s => if(check(s)) run(s) else printlnIO("BOOM"))
  }

  def go: IO[Unit] = for {
    _   <- printlnIO("")
    _   <- printIO("calc> ")
    cmd <- readLineIO
    _   <- commands
            .reduceMapK(runCmd)
            .runA(cmd)
  } yield ()

}
