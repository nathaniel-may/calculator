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

  def notNullAnd(s: String)(f: String => Boolean): Boolean =
    s != null && f(s)

  val commands: NonEmptyList[(String => Boolean, String => IO[Unit])] = NonEmptyList.of(
    (_ == null,                       _   => printlnIO("")),
    (notNullAnd(_)(_.trim == "exit"), _   => IO(())),
    (notNullAnd(_)(_.trim == ":q"),   _   => IO(())),
    (notNullAnd(_)(_ => true),        cmd => printResult(calculate(cmd))._1 >> go)
  )

  def runCmd(in: (String => Boolean, String => IO[Unit])): RunCMD = {
    val (check, run) = in
    StateT.inspectF { s => if (check(s)) Some(run(s)) else None }
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
