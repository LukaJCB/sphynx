package sphynx

import minitest.SimpleTestSuite
import cats.implicits._
import cats._
import cats.data._
import cats.effect._
import mainecoon.{CartesianK, FunctorK}

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

  def rebuildPutGet(info: KVStoreInfo, interp: KVStore[IO]): IO[KVStore[IO]] =
    info.queries.toList.filterNot(info.cache.contains)
      .parTraverse(key => interp.get(key).map(_.map(s => (key, s))))
      .map { list =>
        val table: Map[String, String] = list.flatten.toMap

        new KVStore[IO] {
          def get(key: String) = table.get(key).orElse(info.cache.get(key)) match {
            case Some(a) => Option(a).pure[IO]
            case None => interp.get(key)
          }

          def put(key: String, a: String): IO[Unit] = interp.put(key, a)
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
      info.queries.toList.filterNot(info.cache.contains)
        .parTraverse(key => interp.get(key).map(_.map(s => (key, s))))
        .map { list =>
          val table: Map[String, String] = list.flatten.toMap

          new KVStore[IO] {
            def get(key: String) = table.get(key).orElse(info.cache.get(key)) match {
              case Some(a) => Option(a).pure[IO]
              case None => interp.get(key)
            }

            def put(key: String, a: String): IO[Unit] = interp.put(key, a)
          }
        }
  }


  def kVStorePutGetMonadEliminizer[F[_]: Sync]: MonadOptimizer[KVStore, F] = new MonadOptimizer[KVStore, F] {
    type M = Map[String, String]

    def monoidM = implicitly

    def monadF = implicitly

    def functorK: FunctorK[KVStore] = implicitly
    def semigroupalK: CartesianK[KVStore] = implicitly

    def rebuild(interp: KVStore[F]): KVStore[Kleisli[F, M, ?]] = new KVStore[Kleisli[F, M, ?]] {
      def get(key: String): Kleisli[F, M, Option[String]] = Kleisli(m => m.get(key) match {
        case o @ Some(_) => Applicative[F].pure(o)
        case None => interp.get(key)
      })

      def put(key: String, a: String): Kleisli[F, M, Unit] = Kleisli(m => interp.put(key, a))
    }

    def extract: KVStore[? => M] = new KVStore[? => M] {
      def get(key: String): Option[String] => M = {
        case Some(s) => Map(key -> s)
        case None => Map.empty
      }

      def put(key: String, a: String): Unit => M =
        _ => Map(key -> a)
    }

  }

  test("MonadOptimizer should optimize duplicates and put-get-elimination") {
    implicit val optimize: MonadOptimizer[KVStore, IO] = kVStorePutGetMonadEliminizer[IO]

    val interp = CrazyInterpreter.create.unsafeRunSync()

    val result = optimize.optimizeM(Programs.monadProgram).apply(interp).unsafeRunSync()


    assertEquals(interp.searches.size, 2)
    assertEquals(interp.inserts.size, 3)

    val control = Programs.monadProgram(interp).unsafeRunSync()

    assertEquals(result, control)

  }

  test("Optimizer should optimize duplicates and put-get-elimination") {

    implicit val optimize: Optimizer[KVStore, IO] = kVStorePutGetEliminizer

    val interp = CrazyInterpreter.create.unsafeRunSync()

    val result = optimize
      .optimize(Programs.putGetProgram)(interp)
      .unsafeRunSync()

    assertEquals(interp.searches.size, 2)
    assertEquals(interp.inserts.size, 2)

    val control = Programs.putGetProgram(interp).unsafeRunSync()

    assertEquals(result, control)


  }

  test("SemigroupalOptimizer duplicates should be removed") {

    implicit val optimizer: SemigroupalOptimizer[KVStore, IO] = kVStoreIOOptimizer

    val interp = CrazyInterpreter.create.unsafeRunSync()

    val result = optimizer.nonEmptyOptimize(Programs.applicativeProgram)(interp).unsafeRunSync()

    assertEquals(interp.inserts.size, 1)
    assertEquals(interp.searches.size, 2)

    val control = Programs.applicativeProgram(interp).unsafeRunSync()

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

    val interp = CrazyInterpreter.create.unsafeRunSync()

    val result = optimizer.optimize(p)(interp).unsafeRunSync()

    assertEquals(interp.searches.size, 3)

    val control = program(interp).unsafeRunSync()

    assertEquals(result, control)
  }
}
