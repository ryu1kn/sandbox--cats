package sandbox

import sandbox.helper.Specification

class TraverseTest extends Specification {
  import cats.Applicative
  import cats.syntax.apply._

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(Applicative[F].pure(List.empty[B])) {
      (accum, item) => (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  "For Vector" should {
    import cats.instances.vector._

    "produce all combinations" in {
      listSequence(List(Vector(1, 2), Vector(3, 4))) shouldEqual
        Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
    }

    "produce all combinations for 3" in {
      listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) shouldEqual
        Vector(
          List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6),
          List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6)
        )
    }
  }

  "For Option" should {
    import cats.instances.option._

    def process(inputs: List[Int]): Option[List[Int]] =
      listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

    "return list of inputs with all even numbers" in {
      process(List(2, 4, 6)) shouldEqual Some(List(2, 4, 6))
    }

    "return nothing given odd numbers" in {
      process(List(1, 2, 3)) shouldEqual None
    }
  }

  "For Validated" should {
    import cats.data.Validated
    import cats.instances.list._

    type ErrorsOr[A] = Validated[List[String], A]

    def process(inputs: List[Int]): ErrorsOr[List[Int]] =
      listTraverse(inputs) { n =>
        if (n % 2 == 0) Validated.valid(n)
        else Validated.invalid(List(s"$n is not even"))
      }

    "return a valid value" in {
      process(List(2, 4, 6)) shouldEqual Validated.valid(List(2, 4, 6))
    }

    "return error messages" in {
      process(List(1, 2, 3)) shouldEqual Validated.invalid(List("1 is not even", "3 is not even"))
    }
  }
}
