package sphynx

import cats.{Applicative, Monoid}
import cats.data.Const

trait StaticOptimizer[Alg[_[_]], F[_]] {
  type M

  def monoidM: Monoid[M]
  def applicativeF: Applicative[F]

  def extract: Alg[Const[M, ?]]
  def rebuild(m: M, interpreter: Alg[F]): Alg[F]

  def optimize[A](p: ApplicativeProgram[Alg, A]): Alg[F] => F[A] = { interpreter =>
    implicit val M: Monoid[M] = monoidM
    implicit val F: Applicative[F] = applicativeF
    val m: M = p(extract).getConst

    p(rebuild(m, interpreter))
  }
}



object StaticOptimizer {

  def apply[Alg[_[_]], F[_]](implicit ev: StaticOptimizer[Alg, F]): StaticOptimizer[Alg, F] = ev

  implicit class OptimizerOps[Alg[_[_]], A](val value: ApplicativeProgram[Alg, A]) extends AnyVal {
    def optimize[F[_]: Applicative](interp: Alg[F])(implicit O: StaticOptimizer[Alg, F]): F[A] = O.optimize(value)(interp)
  }
}
