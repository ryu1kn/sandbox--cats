package sandbox.part1

import sandbox.helper.Specification

class MonoidTest extends Specification {
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
  }

  "Boolean OR operation" should {
    implicit val monoidOverOr = new Monoid[Boolean] {
      override def empty: Boolean = false

      override def combine(x: Boolean, y: Boolean): Boolean = x || y
    }

    implicit class MonoidBooleanOps[A](value: A) {
      def combine(other: A)(implicit m: Monoid[A]): A = m.combine(value, other)
    }

    "Empty operation" in {
      Monoid.apply.empty.combine(true) shouldEqual true
      Monoid.apply.empty.combine( false) shouldEqual false
      true.combine(Monoid.apply.empty) shouldEqual true
      false.combine(Monoid.apply.empty) shouldEqual false
    }

    "associativity" in {
      true.combine(true).combine(false) shouldEqual true.combine(true.combine(false))
    }
  }

  "Boolean AND operation" should {
    implicit val monoidOverAnd = new Monoid[Boolean] {
      override def empty: Boolean = true

      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }

    implicit class MonoidBooleanOps[A](value: A) {
      def combine(other: A)(implicit m: Monoid[A]): A = m.combine(value, other)
    }

    "Empty operation" in {
      Monoid.apply.empty.combine(true) shouldEqual true
      Monoid.apply.empty.combine( false) shouldEqual false
      true.combine(Monoid.apply.empty) shouldEqual true
      false.combine(Monoid.apply.empty) shouldEqual false
    }

    "associativity" in {
      true.combine(true).combine(false) shouldEqual true.combine(true.combine(false))
    }
  }
}
