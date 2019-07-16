package sandbox

import org.scalatest._

class PrintableTest extends WordSpec with Matchers {

  "Printable" should {
    import PrintableInstances._

    "give a string representation" in {
      Printable.format(4) shouldEqual "4"
    }

    "print a string representation" in {
      Printable.print(4)
    }
  }

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
}
