package sandbox.part1

import sandbox.helper.Specification

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

class WriterTest extends Specification {

  import cats.data.Writer
  import cats.instances.vector._
  import cats.syntax.applicative._
  import cats.syntax.writer._

  def slowly[A](body: => A): A = try body finally Thread.sleep(100)

  type Logged[A] = Writer[Vector[String], A]

  def factorial(n: Int): Logged[Int] =
    for {
      ans <- slowly(if (n == 0) 1.pure[Logged] else factorial(n - 1).map(_ * n))
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  "Writer monad" should {
    "accumulate logs" in {
      val Vector((logA, ansA), (logB, ansB)) =
        Await.result(Future.sequence(Vector(
          Future(factorial(3).run),
          Future(factorial(5).run)
        )), 5.seconds)
      logA shouldEqual Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6")
      ansA shouldEqual 6
      logB shouldEqual Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6", "fact 4 24", "fact 5 120")
      ansB shouldEqual 120
    }
  }
}
