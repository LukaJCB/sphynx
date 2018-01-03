package sphynx

import cats.effect._

object CrazyInterpreter extends KVStore[IO] {

  var searches: Map[String, Int] = Map.empty
  var inserts: Map[String, Int] = Map.empty


  def get(key: String) = IO {
    searches = searches.updated(key, searches.get(key).getOrElse(0) + 1)
    Option(key + "!")
  }

  def put(key: String, a: String) = IO {
    inserts = inserts.updated(key, inserts.get(key).getOrElse(0) + 1)
  }
}
