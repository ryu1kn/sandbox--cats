package sandbox

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
}
