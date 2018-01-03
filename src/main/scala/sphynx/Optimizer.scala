package sphynx

import cats.{FlatMap, Monad, Monoid, Semigroup}

trait Optimizer[Alg[_[_]], F[_]] extends SemigroupalOptimizer[Alg, F] {

  def semigroupM: Semigroup[M] = monoidM
  def flatMapF: Monad[F] = monadF

  def monoidM: Monoid[M]
  def monadF: Monad[F]

  def optimize[A](p: ApplicativeProgram[Alg, A]): Alg[F] => F[A] = { interpreter =>
    implicit val M: Monoid[M] = monoidM
    implicit val F: Monad[F] = monadF
    val m: M = p(extract).getConst

    Monad[F].flatMap(rebuild(m, interpreter))(interp => p(interp))
  }

}

object Optimizer {

  def apply[Alg[_[_]], F[_]](implicit ev: Optimizer[Alg, F]): Optimizer[Alg, F] = ev

  implicit class OptimizerOps[Alg[_[_]], A](val value: ApplicativeProgram[Alg, A]) extends AnyVal {
    def optimize[F[_]: Monad](interp: Alg[F])(implicit O: Optimizer[Alg, F]): F[A] = O.optimize(value)(interp)
  }
}
