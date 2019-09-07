package sandbox.part1

import sandbox.helper.Specification

class SuperAdderTest extends Specification {
  import cats.instances.int._
  import cats.instances.option._

  "SuperAdder" should {
    "return 0 if no numbers are given" in {
      SuperAdder.add(List[Int]()) shouldEqual 0
    }

    "return the given number if given only one" in {
      SuperAdder.add(List(4)) shouldEqual 4
    }

    "add numbers if given multiple" in {
      SuperAdder.add(List(4, 2)) shouldEqual 6
    }

    "add optional values" in {
      SuperAdder.add(List(Some(4), None)) shouldEqual Some(4)
    }
  }

  "SuperAdder for custom type" should {
    import cats.Monoid
    import cats.instances.double._
    import cats.syntax.semigroup._

    case class Order(totalCost: Double, quantity: Double)

    implicit val orderMonoid = new Monoid[Order] {
      override def empty: Order = Order(0, 0)

      override def combine(x: Order, y: Order): Order =
        Order(x.totalCost |+| y.totalCost, x.quantity |+| y.quantity)
    }

    "add custom type" in {
      SuperAdder.add(List(Order(100, 1), Order(50, 3))) shouldEqual Order(150, 4)
    }
  }
}
