package sandbox.part1

import sandbox.helper.Specification

class SemigroupalTest extends Specification {

//  "My product" should {
//    import cats.Monad
//    import cats.syntax.flatMap._
//    import cats.syntax.functor._
//
//    def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
//      x.flatMap(a => y.map(b => (a, b)))
//  }

  "Validated" should {
    import cats.data.Validated
    import cats.instances.list._
    import cats.syntax.apply._
    import cats.syntax.either._

    case class User(name: String, age: Int)

    type FormData = Map[String, String]
    type FailFast[A] = Either[List[String], A]
    type FailSlow[A] = Validated[List[String], A]
    type NumFmtExt = NumberFormatException

    def getValue(name: String)(data: FormData): FailFast[String] =
      data.get(name).toRight(List(s"$name field not specified"))

    def parseInt(name: String)(data: String): FailFast[Int] =
      Either.catchOnly[NumFmtExt](data.toInt)
        .leftMap(_ => List(s"$name must be an integer"))

    def nonBlank(name: String)(data: String): FailFast[String] =
      Right(data).ensure(List(s"$name cannot be blank"))(_.nonEmpty)

    def nonNegative(name: String)(data: Int): FailFast[Int] =
      Right(data).ensure(List(s"$name must be non-negative"))(_ >= 0)

    def readName(data: FormData): Either[List[String], String] =
      getValue("name")(data).flatMap(nonBlank("name"))

    def readAge(data: Map[String, String]): Either[List[String], Int] =
      getValue("age")(data)
        .flatMap(parseInt("age"))
        .flatMap(nonNegative("age"))

    def readUser(data: FormData): FailSlow[User] =
      (
        readName(data).toValidated,
        readAge(data).toValidated
      ).mapN(User.apply)

    "read a name" in {
      readName(Map("name" -> "foo")) shouldEqual Right("foo")
    }

    "fail to read a name" in {
      readName(Map()) shouldEqual Left(List("name field not specified"))
    }

    "parse an integer" in {
      readAge(Map("age" -> "11")) shouldEqual Right(11)
    }

    "fail to parse an integer" in {
      readAge(Map("age" -> "foo")) shouldEqual Left(List("age must be an integer"))
    }

    "fail when given an empty string" in {
      readName(Map("name" -> "")) shouldEqual Left(List("name cannot be blank"))
    }

    "passthrough a positive integer" in {
      nonNegative("age")(11) shouldEqual Right(11)
    }

    "fail when given a negative integer" in {
      readAge(Map("age" -> "-1")) shouldEqual Left(List("age must be non-negative"))
    }

    "parse a user info" in {
      readUser(Map("name" -> "foo", "age" -> "11")) shouldEqual Validated.valid(User("foo", 11))
    }

    "give all validation errors" in {
      readUser(Map("age" -> "-1")) shouldEqual Validated.invalid(List("name field not specified", "age must be non-negative"))
    }
  }
}
