package sphynx.syntax

import cats.Monad
import sphynx.{ApplicativeProgram, Optimizer}

trait OptimizerSyntax {
  implicit def optimizerOps[Alg[_[_]], A](value: ApplicativeProgram[Alg, A]): OptimizerOps[Alg, A] =
    new OptimizerOps[Alg, A](value)
}

class OptimizerOps[Alg[_[_]], A](val value: ApplicativeProgram[Alg, A]) extends AnyVal {
  def optimize[F[_]: Monad](interp: Alg[F])(implicit O: Optimizer[Alg, F]): F[A] = O.optimize(value)(interp)
}

object optimizer extends OptimizerSyntax
