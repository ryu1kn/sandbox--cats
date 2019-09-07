package sandbox.part1

import sandbox.part1.helper.Specification

class FoldTest extends Specification {
  "foldLeft" should {
    "join from left" in {
      List(1, 2, 3).foldLeft(List.empty[Int]) { (a, i) => i :: a } shouldEqual List(3, 2, 1)
    }
  }

  "foldRight" should {
    "join from right" in {
      List(1, 2, 3).foldRight(List.empty[Int]) { (i, a) => i :: a } shouldEqual List(1, 2, 3)
    }
  }

  "foldLeft with cats" should {
    import cats.Foldable

    "fold list items from left" in {
      import cats.instances.list._
      Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) shouldEqual 6
    }

    "fold option" in {
      import cats.instances.option._
      Foldable[Option].foldLeft(Option(1), 10)(_ + _) shouldEqual 11
    }
  }
}
