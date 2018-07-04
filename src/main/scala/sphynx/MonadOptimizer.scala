package sphynx

import cats.{Id, Monad, Monoid, Semigroup, ~>}
import cats.data._
import mainecoon.{FunctorK, CartesianK}


trait MonadOptimizer[Alg[_[_]], F[_]] {

  type M

  def monoidM: Monoid[M]
  def monadF: Monad[F]

  def functorK: FunctorK[Alg]
  def semigroupalK: CartesianK[Alg]

  def rebuild(interp: Alg[F]): Alg[Kleisli[F, M, ?]]

  def extract: Alg[? => M]


  def optimizeM[A](p: Program[Alg, Monad, A]): Alg[F] => F[A] = { interpreter =>
    implicit val M: Monoid[M] = monoidM
    implicit val F: Monad[F] = monadF

    type Prod[A] = Tuple2K[Kleisli[F, M, ?], ? => M, A]

    val productAlg =
      semigroupalK.productK[Kleisli[F, M, ?], ? => M](rebuild(interpreter), extract)

    val withState: Alg[StateT[F, M, ?]] =
      functorK.mapK(productAlg)(new (Prod ~> StateT[F, M, ?]) {
        def apply[A](fa: Prod[A]): StateT[F, M, A] =
          StateT(m => F.map(fa.first.run(m))(a => M.combine(fa.second(a), m) -> a))
      })

    p(withState).runEmptyA
  }

}
