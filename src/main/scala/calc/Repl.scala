package calc

import calc.Main.{calculate, printIO, printResult, printlnIO, readLineIO}
import cats.data.{NonEmptyList, StateT}
import cats.effect.IO
import cats.syntax.all._
import cats.instances.all._

object Repl {
  type RunCMD = StateT[Option, String, IO[Unit]]
  val header: String = ":::: This is the calc repl - type `exit` to quit ::::"

  val repl: IO[Unit] =
    printlnIO(header) *> go

  val commands: NonEmptyList[(String => Boolean, String => IO[Unit])] = NonEmptyList.of(
    (_ == null,        _   => printlnIO("")),
    (_.trim == "exit", _   => IO(())),
    (_.trim == ":q",   _   => IO(())),
    (_ => true,        cmd => printResult(calculate(cmd))._1 >> go)
  )

  def runCmd(in: (String => Boolean, String => IO[Unit])): RunCMD = {
    val (check, run) = in
    StateT.inspectF((s: String) => if (check(s)) Some (run(s)) else None)
  }

  def go: IO[Unit] = for {
    _   <- printlnIO("")
    _   <- printIO("calc> ")
    cmd <- readLineIO
    _   <- commands
            .reduceMapK(runCmd)
            .runA(cmd)
            .getOrElse(IO(()))
  } yield ()

}
