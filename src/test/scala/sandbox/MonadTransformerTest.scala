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
  }
}
