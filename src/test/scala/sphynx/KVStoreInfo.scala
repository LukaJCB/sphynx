package sphynx

import cats.Monoid
import cats.implicits._

case class KVStoreInfo(queries: Set[String], cache: Map[String, String])

object KVStoreInfo {
  implicit val infoMonoid: Monoid[KVStoreInfo] = new Monoid[KVStoreInfo] {
    def combine(a: KVStoreInfo, b: KVStoreInfo): KVStoreInfo =
      KVStoreInfo(a.queries |+| b.queries, a.cache |+| b.cache)

    def empty: KVStoreInfo = KVStoreInfo(Set.empty, Map.empty)
  }
}
