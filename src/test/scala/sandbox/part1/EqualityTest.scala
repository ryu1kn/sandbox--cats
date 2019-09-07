package sandbox.part1

import sandbox.part1.helper.{Cat, Specification}

class EqualityTest extends Specification {
  "Eq" should {
    import cats.Eq
    import cats.instances.int._
    import cats.instances.option._
    import cats.instances.string._
    import cats.syntax.eq._

    // Eq's === collide with Scalatest's ===; hence using `eqv` instead
    implicit val catEq: Eq[Cat] =
      Eq.instance[Cat]((c1, c2) => c1.age.eqv(c2.age) && c1.color.eqv(c2.color) && c1.name.eqv(c2.name))

    val blackCat = Cat("A", 1, "black")
    val whiteCat = Cat("A", 1, "white")

    "tell they are the same" in {
      blackCat.eqv(blackCat) shouldBe true
    }

    "tell they are not the same" in {
      (blackCat =!= whiteCat) shouldBe true
    }

    "tell they are the same for optional values" in {
      (Option(blackCat) =!= Option.empty[Cat]) shouldBe true
    }

    "tell they are not the same for optional values" in {
      Option(blackCat).eqv(Option(blackCat)) shouldBe true
    }
  }
}
