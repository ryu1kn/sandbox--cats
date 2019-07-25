package sandbox

import sandbox.helper.Specification

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class WriterTest extends Specification {
  def slowly[A](body: => A): A = try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  "Writer monad" should {
    "WIP: accumulate logs" in {
      Await.result(Future.sequence(Vector(
        Future(factorial(3)),
        Future(factorial(3))
      )), 5.seconds)
    }
  }
}
