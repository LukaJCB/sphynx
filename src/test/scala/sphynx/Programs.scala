package sphynx

import cats.{Applicative, Apply, Monad}
import cats.implicits._

object Programs {


  private def putGetProgramF[F[_]: Applicative](F: KVStore[F]): F[List[String]] =
    List("Cat" -> "Cat!", "Dog" ->"Dog!").traverse(t => F.put(t._1, t._2)) *>
      List("Dog", "Bird", "Mouse", "Bird").traverse(F.get).map(_.flatten)


  val putGetProgram = new ApplicativeProgram[KVStore, List[String]] {
    def apply[F[_]: Applicative](alg: KVStore[F]): F[List[String]] = putGetProgramF(alg)
  }


  private def applicativeProgramF[F[_]: Apply](F: KVStore[F]): F[List[String]] =
    (F.get("Cats"), F.get("Dogs"), F.put("Mice", "mouse"), F.get("Cats"))
      .mapN((f, s, _, t) => List(f, s, t).flatten)

  val applicativeProgram = new ApplyProgram[KVStore, List[String]] {
    def apply[F[_]: Apply](alg: KVStore[F]): F[List[String]] = applicativeProgramF(alg)
  }

  private def monadProgramF[F[_]: Monad](F: KVStore[F]): F[List[String]] = for {
    _ <- F.put("dog", "Daawwwwgg")
    dog <- F.get("dog")
    list <- putGetProgramF(F)
  } yield list

  val monadProgram = new MonadProgram[KVStore, List[String]] {
    def apply[F[_]: Monad](alg: KVStore[F]): F[List[String]] = monadProgramF(alg)
  }

}
