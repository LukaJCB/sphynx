package sphynx

import minitest.SimpleTestSuite
import cats.implicits._
import cats._
import cats.data._
import cats.effect._

object OptimizerTest extends SimpleTestSuite {

  def kVStoreIOOptimizer: Optimizer[KVStore, IO] = new Optimizer[KVStore, IO] {
    type M = Set[String]

    def monoidM = implicitly

    def monadF = implicitly

    def extract = new KVStore[Const[Set[String], ?]] {
      def get(key: String) = Const(Set(key))
      def put(key: String, a: String): Const[Set[String], Unit] = Const(Set.empty)
    }

    def rebuild(gs: Set[String], interp: KVStore[IO]): IO[KVStore[IO]] =
      gs.toList
        .traverse(key => interp.get(key).map(_.map(s => (key, s))))
        .map(_.collect { case Some(v) => v }.toMap)
        .map { m =>
          new KVStore[IO] {
            override def get(key: String) = m.get(key) match {
              case Some(a) => Option(a).pure[IO]
              case None => interp.get(key)
            }

            def put(key: String, a: String): IO[Unit] = interp.put(key, a)
          }
        }

  }

  test("SemigroupalOptimizer duplicates should be removed") {

    implicit val optimizer: SemigroupalOptimizer[KVStore, IO] = kVStoreIOOptimizer

    def program[F[_]: Apply](F: KVStore[F]): F[List[String]] =
      (F.get("Cats"), F.get("Dogs"), F.put("Mice", "mouse"), F.get("Cats"))
        .mapN((f, s, _, t) => List(f, s, t).flatten)


    val p = new ApplyProgram[KVStore, List[String]] {
      def apply[F[_]: Apply](alg: KVStore[F]): F[List[String]] = program(alg)
    }

    val result = optimizer.nonEmptyOptimize(p)(CrazyInterpreter).unsafeRunSync()

    assertEquals(CrazyInterpreter.inserts.size, 1)
    assertEquals(CrazyInterpreter.searches.size, 2)

    val control = program(CrazyInterpreter).unsafeRunSync()

    assertEquals(result, control)
  }


  test("Optimizer duplicates should be removed") {

    implicit val optimizer: Optimizer[KVStore, IO] = kVStoreIOOptimizer

    def program[F[_]: Applicative](F: KVStore[F]): F[List[String]] =
      List(F.get("Cats"), F.get("Dogs"), F.get("Cats"), F.get("Birds"))
        .sequence
        .map(_.collect { case Some(s) => s })


    val p = new ApplicativeProgram[KVStore, List[String]] {
      def apply[F[_]: Applicative](alg: KVStore[F]): F[List[String]] = program(alg)
    }

    val result = optimizer.optimize(p)(CrazyInterpreter).unsafeRunSync()

    assertEquals(CrazyInterpreter.searches.size, 3)

    val control = program(CrazyInterpreter).unsafeRunSync()

    assertEquals(result, control)
  }
}
