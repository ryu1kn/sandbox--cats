package sandbox

import sandbox.helper.Specification

class StateTest extends Specification {

  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case n => operand(n.toInt)
  }

  def operator(f: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case n :: m :: tail => (tail, f(m, n))
      case _ => sys.error("Fail!")
    }

  def operand(value: Int): CalcState[Int] =
    State[List[Int], Int] { oldStack => (value :: oldStack, value) }

  "State Monad" should {
    "add two numbers" in {
      val programme = for {
        _ <- evalOne("1")
        _ <- evalOne("2")
        res <- evalOne("+")
      } yield res
      val (stack, answer) = programme.run(List[Int]()).value
      stack shouldEqual List()
      answer shouldEqual 3
    }

    "subtract numbers" in {
      val programme = for {
        _ <- evalOne("1")
        _ <- evalOne("2")
        res <- evalOne("-")
      } yield res
      val (stack, answer) = programme.run(List[Int]()).value
      stack shouldEqual List()
      answer shouldEqual -1
    }
  }
}
