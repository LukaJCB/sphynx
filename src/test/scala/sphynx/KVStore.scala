package sphynx

trait KVStore[F[_]] {
  def get(key: String): F[Option[String]]
  def put(key: String, a: String): F[Unit]
}
