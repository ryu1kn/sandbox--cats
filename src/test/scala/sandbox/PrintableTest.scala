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
}
