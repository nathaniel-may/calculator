package meta

import org.scalacheck.Prop.forAllNoShrink
import org.scalacheck.{Arbitrary, Properties}
import util.Generators._

import scala.annotation.tailrec
import calc.Calculator.{Tok, Number, Operation}


object GeneratorProperties extends Properties("generators") {
  implicit val arbOp: Arbitrary[Operation] = Arbitrary(opGen)

  property("seq is always in the form { num (op num)* }") = forAllNoShrink(seqGen) {
    seq: List[Tok] =>
      @tailrec
      def isAlternating(l: List[Either[_, _]]): Boolean = l match {
        case Nil                         => true
        case Left(_) :: Nil              => true
        case Left(_) :: Right(_) :: tail => isAlternating(tail)
        case _                           => false
      }

      isAlternating(seq.map{
        case Number(num)   => Left(num)
        case Operation(op) => Right(op)
      })
  }
}
