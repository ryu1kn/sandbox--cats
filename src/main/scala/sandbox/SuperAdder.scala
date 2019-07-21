package sandbox

import cats.Monoid
import cats.syntax.semigroup._

object SuperAdder {
  def add[A: Monoid](items: List[A]): A = items.foldRight(Monoid[A].empty)(_ |+| _)
}
