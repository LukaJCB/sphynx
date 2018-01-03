package sphynx

import cats.arrow.FunctionK
import cats.{Applicative, Monad, Monoid, FlatMap, Semigroup}
import cats.data.Const


trait Program[Alg[_[_]], Constraint[_[_]], A] {
  def apply[F[_]: Constraint](interp: Alg[F]) : F[A]
}
