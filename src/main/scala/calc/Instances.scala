package calc

import cats.{SemigroupK, Monad}
import cats.data.IndexedStateT
import cats.syntax.all._

import scala.util.Try

object Instances {

  implicit val semigroupKTry: SemigroupK[Try] = new SemigroupK[Try] {
    override def combineK[A](x: Try[A], y: Try[A]): Try[A] =
      x orElse y
  }

  implicit def indexedStateTSemigroupK[F[_] : Monad : SemigroupK, S] = new SemigroupK[({type L[A] = IndexedStateT[F, S, S, A]})#L] {
    override def combineK[A](x: IndexedStateT[F, S, S, A], y: IndexedStateT[F, S, S, A]): IndexedStateT[F, S, S, A] =
      IndexedStateT[F, S, S, A](s => x.run(s) <+> y.run(s))
  }

}
