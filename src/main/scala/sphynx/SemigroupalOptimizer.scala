package sphynx

import cats.{FlatMap, Monad, Semigroup}
import cats.data.Const

trait SemigroupalOptimizer[Alg[_[_]], F[_]] {

  type M

  def semigroupM: Semigroup[M]
  def flatMapF: FlatMap[F]

  def extract: Alg[Const[M, ?]]
  def rebuild(m: M, interpreter: Alg[F]): F[Alg[F]]

  def nonEmptyOptimize[A](p: ApplyProgram[Alg, A]): Alg[F] => F[A] = { interpreter =>
    implicit val M: Semigroup[M] = semigroupM
    implicit val F: FlatMap[F] = flatMapF
    val m: M = p(extract).getConst

    FlatMap[F].flatMap(rebuild(m, interpreter))(interp => p(interp))
  }

}

object SemigroupalOptimizer {

  def apply[Alg[_[_]], F[_]](implicit ev: SemigroupalOptimizer[Alg, F]): SemigroupalOptimizer[Alg, F] = ev

  implicit class SemigroupalOptimizerOps[Alg[_[_]], A](val value: ApplyProgram[Alg, A]) extends AnyVal {
    def nonEmptyOptimize[F[_]: FlatMap](interp: Alg[F])(implicit O: SemigroupalOptimizer[Alg, F]): F[A] =
      O.nonEmptyOptimize(value)(interp)
  }
}
