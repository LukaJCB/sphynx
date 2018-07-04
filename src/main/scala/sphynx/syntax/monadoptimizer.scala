package sphynx.syntax

import cats.Monad
import sphynx.{MonadProgram, MonadOptimizer}

trait MonadSyntax {
  implicit def monadOptimizerOps[Alg[_[_]], A](value: MonadProgram[Alg, A]): MonadOptimizerOps[Alg, A] =
    new MonadOptimizerOps(value)
}

class MonadOptimizerOps[Alg[_[_]], A](val value: MonadProgram[Alg, A]) extends AnyVal {
  def optimizeM[F[_]: Monad](interp: Alg[F])(implicit O: MonadOptimizer[Alg, F]): F[A] =
    O.optimizeM(value)(interp)
}

object monadoptimizer extends MonadSyntax
