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

  case class KVStoreInfo(gets: Set[String], puts: Map[String, String])

  object KVStoreInfo {
    implicit val infoMonoid: Monoid[KVStoreInfo] = new Monoid[KVStoreInfo] {
      def combine(a: KVStoreInfo, b: KVStoreInfo): KVStoreInfo =
        KVStoreInfo(a.gets |+| b.gets, a.puts |+| b.puts)

      def empty: KVStoreInfo = KVStoreInfo(Set.empty, Map.empty)
    }
  }

  def kVStorePutGetEliminizer: Optimizer[KVStore, IO] = new Optimizer[KVStore, IO] {
    type M = KVStoreInfo

    def monoidM = implicitly

    def monadF = implicitly

    def extract = new KVStore[Const[KVStoreInfo, ?]] {
      def get(key: String): Const[KVStoreInfo, Option[String]] =
        Const(KVStoreInfo(Set(key), Map.empty))

      def put(key: String, a: String): Const[KVStoreInfo, Unit] =
        Const(KVStoreInfo(Set.empty, Map(key -> a)))
    }

    def rebuild(info: KVStoreInfo, interp: KVStore[IO]): IO[KVStore[IO]] =
      info.gets.toList.filterNot(info.puts.contains)
        .parTraverse(key => interp.get(key).map(_.map(s => (key, s))))
        .map { list =>
          val table: Map[String, String] = list.flatten.toMap

          new KVStore[IO] {
            def get(key: String) = table.get(key).orElse(info.puts.get(key)) match {
              case Some(a) => Option(a).pure[IO]
              case None => interp.get(key)
            }

            def put(key: String, a: String): IO[Unit] = interp.put(key, a)
          }
        }
  }

  test("Optimizer should optimize duplicates and put-get-elimination") {

    implicit val optimize: Optimizer[KVStore, IO] = kVStorePutGetEliminizer

    val gets = List("Dog", "Bird", "Mouse", "Bird")
    val puts = List("Cat" -> "Cat!", "Dog" ->"Dog!")

    def program[F[_]: Applicative](F: KVStore[F]): F[List[String]] =
      puts.traverse(t => F.put(t._1, t._2)) *> gets.traverse(F.get).map(_.flatten)


    val p = new ApplicativeProgram[KVStore, List[String]] {
      def apply[F[_]: Applicative](alg: KVStore[F]): F[List[String]] = program(alg)
    }


    val result = optimize
      .optimize(p)(CrazyInterpreter)
      .unsafeRunSync()

    assertEquals(CrazyInterpreter.searches.size, 2)
    assertEquals(CrazyInterpreter.inserts.size, 2)

    val control = program(CrazyInterpreter).unsafeRunSync()

    assertEquals(result, control)


  }

  test("SemigroupalOptimizer duplicates should be removed") {

    implicit val optimizer: SemigroupalOptimizer[KVStore, IO] = kVStoreIOOptimizer

    def program[F[_]: Apply](F: KVStore[F]): F[List[String]] =
      (F.get("Cats"), F.get("Dogs"), F.put("Mice", "mouse"), F.get("Cats"))
        .mapN((f, s, _, t) => List(f, s, t).flatten)


    val p = new ApplyProgram[KVStore, List[String]] {
      def apply[F[_]: Apply](alg: KVStore[F]): F[List[String]] = program(alg)
    }

    CrazyInterpreter.flush.unsafeRunSync()

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

    CrazyInterpreter.flush.unsafeRunSync()

    val result = optimizer.optimize(p)(CrazyInterpreter).unsafeRunSync()

    assertEquals(CrazyInterpreter.searches.size, 3)

    val control = program(CrazyInterpreter).unsafeRunSync()

    assertEquals(result, control)
  }
}
