package meta

import org.scalacheck.Prop.forAllNoShrink
import org.scalacheck.{Arbitrary, Properties}
import util.Generators._

import scala.annotation.tailrec
import calc.Calculator.{EvalElem, Op}


object GeneratorProperties extends Properties("generators") {
  implicit val arbOp: Arbitrary[Op] = Arbitrary(opGen)

  property("seq is always in the form { num (op num)* }") = forAllNoShrink(seqGen) {
    seq: List[EvalElem] =>
      @tailrec
      def isAlternating(l: List[Either[_, _]]): Boolean = l match {
        case Nil                         => true
        case Left(_) :: Nil              => true
        case Left(_) :: Right(_) :: tail => isAlternating(tail)
        case _                           => false
      }

      isAlternating(seq.map{
        case Left(num) => Left(num)
        case Right(op) => Right(op)
      })
  }
}
