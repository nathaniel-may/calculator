package calc

import calc.Calculator.{Op, ops}

// Some exception messages require triple quotes and trimming because of https://github.com/scala/bug/issues/6476
object Exceptions {

  class CalcRuntimeException(message: String)     extends Exception(message)
  class CalcCompilationException(message: String) extends Exception(message)

  class DivideByZeroErr(message: String) extends CalcRuntimeException(message) {
    def this() {
      this("cannot divide by zero")
    }
  }

  class EmptyInputErr(message: String) extends CalcCompilationException(message) {
    def this() {
      this("cannot run computation on empty input")
    }
  }

  class InvalidElementErr(message: String) extends CalcCompilationException(message)
  object InvalidElementErr {
    def from(elem: String): InvalidElementErr =
      new InvalidElementErr(s""" "$elem" is not a number or one of the following operators ${ops.mkString(", ")}""".trim)
  }

  class MissingLeftInputErr(message: String) extends CalcCompilationException(message)
  object MissingLeftInputErr {
    def from(op: Op): MissingLeftInputErr =
      new MissingLeftInputErr("cannot start input with an operator: started with \"" + op + "\"")
  }

  class MissingRightInputErr(message: String) extends CalcCompilationException(message)
  object MissingRightInputErr {
    def from(op: Op): MissingRightInputErr =
      new MissingRightInputErr(s"""operator "$op" missing right-hand input""")
  }

  class InvalidSequenceErr(message: String) extends CalcCompilationException(message)
  object InvalidSequenceErr {
    def from(seq: String): InvalidSequenceErr =
      new InvalidSequenceErr(s""" "$seq" is not a valid sequence""".trim)
  }

  class UnknownCompilationErr(message: String) extends CalcCompilationException(message) {
    def this() {
      this("unknown compilation error")
    }
  }

}
