package sandbox.part2_ch8

import cats.Applicative
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._
import sandbox.helper.Specification

import scala.concurrent.Future

object Uptime {
  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  // In the real implementation, UptimeClient returns future value
  class RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int] = ???
  }

  class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }
}

class UptimeTest extends Specification {
  import cats.Id
  import sandbox.part2_ch8.Uptime._

  // In the mock, process is synchronous
  class MockUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    def getUptime(hostname: String): Int = hosts.getOrElse(hostname, 0)
  }

  "UptimeService" should {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new MockUptimeClient(hosts)
    val service = new UptimeService(client)

    "calculate total time" in {
      service.getTotalUptime(hosts.keys.toList) shouldEqual 16
    }
  }
}
