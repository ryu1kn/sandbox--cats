package sandbox

import sandbox.helper.Specification

import scala.concurrent.Await
import scala.concurrent.duration._

class MonadTransformerTest extends Specification {
  import cats.data.EitherT
  import cats.instances.future._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future

  type Response[A] = EitherT[Future, String, A]

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(power) => EitherT.right(Future(power))
      case None => EitherT.left(Future(s"$autobot is unreachable."))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      power1 <- getPowerLevel(ally1)
      power2 <- getPowerLevel(ally2)
    } yield power1 + power2 > 15

  def tacticalReport(ally1: String, ally2: String): String =
    Await.result(canSpecialMove(ally1, ally2).value, 1.second) match {
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => "Jazz and Bumblebee need a recharge"
      case Left(message) => s"Comms error: $message"
    }

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  "Transformer" should {
    "return the power if its readable" in {
      Await.result(getPowerLevel("Bumblebee").value, 1.second) shouldEqual Right(8)
    }

    "return an error message if the autobot is unreadable" in {
      Await.result(getPowerLevel("Bumbleboo").value, 1.second) shouldEqual Left("Bumbleboo is unreachable.")
    }

    "tell if they can perform special move" in {
      Await.result(canSpecialMove("Bumblebee", "Hot Rod").value, 1.second) shouldEqual Right(true)
    }

    "report that they can perform special move" in {
      tacticalReport("Bumblebee", "Hot Rod") shouldEqual "Bumblebee and Hot Rod are ready to roll out!"
    }

    "report that they need a recharge" in {
      tacticalReport("Jazz", "Bumblebee") shouldEqual "Jazz and Bumblebee need a recharge"
    }

    "report that unreachable autobot" in {
      tacticalReport("Jazz", "Ironhide") shouldEqual "Comms error: Ironhide is unreachable."
    }
  }
}
