package sandbox

import sandbox.helper.{Cat, Specification}

class ShowTest extends Specification {

  "Printable for Cat" should {
    import PrintableInstances._

    implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
      override def format(value: Cat): String = {
        val name = Printable.format(value.name)
        val age = Printable.format(value.age)
        val color = Printable.format(value.color)
        s"$name is a $age year-old $color cat."
      }
    }

    // Printable definition never talked about Cat, but can handle a cat
    "describe a cat" in {
      Printable.format(Cat("Meow", 2, "white")) shouldEqual "Meow is a 2 year-old white cat."
    }
  }

  "Show" should {
    import cats.Show
    import cats.instances.int._
    import cats.instances.string._
    import cats.syntax.show._

    implicit val dateShow: Show[Cat] =
      Show.show(cat => s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat.")

    "stringify cats" in {
      Cat("Meow", 2, "white").show shouldEqual "Meow is a 2 year-old white cat."
    }
  }
}
