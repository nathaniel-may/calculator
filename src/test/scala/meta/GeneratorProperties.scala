package meta

import org.scalacheck.Prop.{forAll, forAllNoShrink}
import org.scalacheck.{Gen, Arbitrary, Properties}, Gen.nonEmptyListOf
import util.Generators._
import scala.annotation.tailrec
import calc.Calculator.Op


object GeneratorProperties extends Properties("generators") {
  implicit val arbOp: Arbitrary[Op] = Arbitrary(opGen)

  property("seq is always in the form { num (op num)* }") = forAllNoShrink(seqGen) {
    seq: List[Either[Double, Op]] =>
      @tailrec
      def isAlternating(l: List[Either[_, _]]): Boolean = l match {
        case Nil                         => true
        case Left(_) :: Nil              => true
        case Left(_) :: Right(_) :: tail => isAlternating(tail)
        case _                           => false
      }

      isAlternating(seq)
  }
}
