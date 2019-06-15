package meta

import org.scalacheck.Prop.forAllNoShrink
import org.scalacheck.{Arbitrary, Properties}
import util.Generators._
import scala.annotation.tailrec
import calc.Calculator.Op
import calc.{CalcTree, Literal, Operator}


object GeneratorProperties extends Properties("generators") {
  implicit val arbOp: Arbitrary[Op] = Arbitrary(opGen)

  property("seq is always in the form { num (op num)* }") = forAllNoShrink(seqGen) {
    seq: List[CalcTree] =>
      @tailrec
      def isAlternating(l: List[Either[_, _]]): Boolean = l match {
        case Nil                         => true
        case Left(_) :: Nil              => true
        case Left(_) :: Right(_) :: tail => isAlternating(tail)
        case _                           => false
      }

      isAlternating(seq.map{
        case Literal(num) => Left(num)
        case Operator(op, _) => Right(op)
        case _ => Left(0) // TODO should be unreachable
      })
  }
}
