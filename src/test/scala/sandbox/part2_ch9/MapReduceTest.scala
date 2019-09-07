package sandbox.part2_ch9

import cats.Monoid
import cats.syntax.monoid._
import sandbox.helper.Specification

import scala.concurrent.Await
import scala.concurrent.duration._

class MapReduceTest extends Specification {
  "MapReduce (Single-threaded)" should {
    def foldMap[A, B: Monoid](values: Vector[A])(fn: A => B): B =
      values.foldLeft(Monoid[B].empty)(_ |+| fn(_))

    "work on integers" in {
      import cats.instances.int._
      foldMap(Vector(1, 2, 3))(identity) shouldEqual 6
    }

    "work on strings" in {
      import cats.instances.string._
      foldMap(Vector(1, 2, 3))(_.toString + "! ") shouldEqual "1! 2! 3! "
    }

    "work on characters" in {
      import cats.instances.string._
      foldMap("Hello World!".toVector)(_.toString.toUpperCase) shouldEqual "HELLO WORLD!"
    }
  }

  "MapReduce (Multi-threaded)" should {
    import cats.instances.future._
    import cats.instances.vector._
    import cats.syntax.foldable._
    import cats.syntax.traverse._

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future

    val cpuCount = Runtime.getRuntime.availableProcessors

    def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
      val groupSize = (1.0 * values.size / cpuCount).ceil.toInt
      values.grouped(groupSize).toVector
        .traverse(group => Future(group.foldMap(func)))
        .map(_.combineAll)
    }

    "work on integers" in {
      import cats.instances.int._
      Await.result(parallelFoldMap((1 to 1000000).toVector)(identity), 1.second) shouldEqual 1784293664
    }
  }
}
