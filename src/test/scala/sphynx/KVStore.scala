package sphynx

import cats.data.Tuple2K
import cats.~>
import mainecoon.{CartesianK, FunctorK}

trait KVStore[F[_]] {
  def get(key: String): F[Option[String]]
  def put(key: String, a: String): F[Unit]
}

object KVStore {
  implicit val applyKForKVStore: FunctorK[KVStore] with CartesianK[KVStore] = new FunctorK[KVStore] with CartesianK[KVStore] {
    def mapK[F[_], G[_]](algf: KVStore[F])(f: ~>[F, G]): KVStore[G] = new KVStore[G] {
      def get(key: String): G[Option[String]] = f(algf.get(key))

      def put(key: String, a: String): G[Unit] = f(algf.put(key, a))
    }

    def productK[F[_], G[_]](af: KVStore[F], ag: KVStore[G]): KVStore[Tuple2K[F, G, ?]] =
      new KVStore[Tuple2K[F, G, ?]] {
        def get(key: String): Tuple2K[F, G, Option[String]] =
          Tuple2K(af.get(key), ag.get(key))

        def put(key: String, a: String): Tuple2K[F, G, Unit] =
          Tuple2K(af.put(key, a), ag.put(key, a))
      }
  }
}
