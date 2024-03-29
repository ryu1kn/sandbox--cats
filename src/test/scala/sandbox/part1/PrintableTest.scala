package sandbox.part1

import sandbox.helper.Specification

class PrintableTest extends Specification {
  "Printable" should {
    import PrintableInstances._

    "give a string representation" in {
      Printable.format(4) shouldEqual "4"
    }

    "print a string representation" in {
      Printable.print(4)
    }
  }

  "Printable via extension" should {
    import PrintableSyntax._
    import PrintableInstances._

    "give a string representation" in {
      4.format shouldEqual "4"
    }
  }
}
