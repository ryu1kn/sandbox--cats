package sandbox.part1

import cats.data.Reader
import sandbox.helper.Specification

class ReaderTest extends Specification {

  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  import cats.syntax.applicative._

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    new DbReader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    new DbReader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    findUsername(userId).flatMap(_.map(checkPassword(_, password)).getOrElse(false.pure[DbReader]))

  "Reader" should {
    val users = Map(1 -> "dade", 2 -> "kate", 3 -> "margo")
    val passwords = Map("dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")
    val db = Db(users, passwords)

    "check login" in {
      checkLogin(1, "zerocool").run(db) shouldEqual true
      checkLogin(4, "davinci").run(db) shouldEqual false
    }
  }
}
