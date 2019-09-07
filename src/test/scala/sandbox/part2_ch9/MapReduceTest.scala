package sandbox.part2_ch9

import cats.Monoid
import cats.syntax.monoid._
import sandbox.helper.Specification

class MapReduceTest extends Specification {
  def foldMap[A, B: Monoid](values: Vector[A])(fn: A => B): B =
    values.foldLeft(Monoid[B].empty)(_ |+| fn(_))

  "MapReduce" should {
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
}
