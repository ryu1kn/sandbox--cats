package sandbox

import sandbox.helper.Specification

class SuperAdderTest extends Specification {

  "SuperAdder" should {
    "return 0 if no numbers are given" in {
      SuperAdder.add(List()) shouldEqual 0
    }

    "return the given number if given only one" in {
      SuperAdder.add(List(4)) shouldEqual 4
    }

    "add numbers if given multiple" in {
      SuperAdder.add(List(4, 2)) shouldEqual 6
    }
  }
}
