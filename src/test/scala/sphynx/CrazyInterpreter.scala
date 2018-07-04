package sphynx

import cats.effect._

trait CrazyInterpreter extends KVStore[IO] {
  def searches: Map[String, Int]
  def inserts: Map[String, Int]
}

object CrazyInterpreter {

  def create: IO[CrazyInterpreter] = IO(new CrazyInterpreter {

    var searches: Map[String, Int] = Map.empty
    var inserts: Map[String, Int] = Map.empty

    def get(key: String) = IO {
      searches = searches.updated(key, searches.get(key).getOrElse(0) + 1)
      Option(key + "!")
    }

    def put(key: String, a: String) = IO {
      inserts = inserts.updated(key, inserts.get(key).getOrElse(0) + 1)
    }
  })
}
