package calc

import calc.Main.{calculate, printIO, printResult, printlnIO}
import cats.data.{NonEmptyList, StateT}
import cats.effect.IO
import cats.syntax.all._
import cats.instances.all._
import jline.console.{ConsoleReader, KeyMap, Operation}

object Repl {
  type RunCMD = StateT[Option, String, IO[Unit]]
  type RunOp = StateT[Option, Operation, IO[Unit]]
  val header: String = ":::: This is the calc repl - type `exit` to quit ::::"
  val reader = new ConsoleReader()
  val km = KeyMap.keyMaps().get("vi-insert")

  val repl: IO[Unit] =
    printlnIO(header) *> go

  def notNullAnd(s: String)(f: String => Boolean): Boolean =
    s != null && f(s)

  val commands: NonEmptyList[(String => Boolean, String => IO[Unit])] = NonEmptyList.of(
    (notNullAnd(_)(_.trim == "exit"), _   => printlnIO("")),
    (notNullAnd(_)(_.trim == ":q"),   _   => printlnIO("")),
    (notNullAnd(_)(_ => true),        cmd => printlnIO("") >> printResult(calculate(cmd))._1 >> go)
  )

  val keyBindings: NonEmptyList[(Operation => Boolean, IO[Unit])] = NonEmptyList.of(
    (_ == Operation.VI_EOF_MAYBE, printlnIO("")),
    (_ => true,                   go)
  )

  def runCmd(checkRun: (String => Boolean, String => IO[Unit])): RunCMD = {
    val (check, run) = checkRun
    StateT.inspectF { s => if (check(s)) Some(run(s)) else None }
  }

  def runOp(checkRun: (Operation => Boolean, IO[Unit])): RunOp = {
    val (check, run) = checkRun
    StateT.inspectF { op => if(check(op)) Some(run) else None }
  }

  def go = {
    def gogo(cum: String): IO[Unit] =
      read flatMap { in => (in match {
        case Left(Operation.ACCEPT_LINE) =>
          commands.reduceMapK(runCmd).runA(cum.reverse)

        case Right(c) =>
          Some(gogo(c + cum))

        case Left(op) =>
          keyBindings.reduceMapK(runOp).runA(op)
      }) getOrElse IO(()) }

    for {
      _ <- printlnIO("")
      _ <- printIO("calc> ")
      _ <- gogo("")
    } yield ()
  }

  def read: IO[Either[Operation, String]] = for {
    char <- IO(reader.readBinding(km))
    in   =  if (char == Operation.SELF_INSERT)
              Right(reader.getLastBinding)
            else
              Left(char match { case op: Operation => op })
    _    <- in match {
              case Right(c) => printIO(c)
              case _        => IO(())
            }
  } yield in

}
