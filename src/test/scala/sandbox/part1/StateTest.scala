package sandbox.part1

import sandbox.helper.Specification

class StateTest extends Specification {

  import cats.data.State
  import cats.syntax.applicative._

  type CalcState[A] = State[List[Int], A]

  def evalInput(input: String): Int = {
    val symbols = input.split(' ').toList
    evalAll(symbols).runA(Nil).value
  }

  def evalAll(symbols: List[String]): CalcState[Int] =
    symbols.foldLeft(0.pure[CalcState]) {
      (step, symbol) => step.flatMap(_ => evalOne(symbol))
    }

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
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

    "evaluate at one" in {
      evalInput("1 2 + 3 4 + *") shouldEqual 21
    }
  }
}
