package sandbox

import cats.Monoid
import cats.instances.int._
import cats.syntax.monoid._

object SuperAdder {
  def add(items: List[Int]): Int = items.foldRight(Monoid[Int].empty)(_ |+| _)
}
