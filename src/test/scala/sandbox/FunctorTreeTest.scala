package sandbox

import cats.Functor
import sandbox.helper.Specification

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)
}

class FunctorTreeTest extends Specification {
  implicit val treeFunctor = new Functor[Tree] {
    override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      case Leaf(v) => Leaf(f(v))
    }
  }

  "Tree" should {
    "be mapped" in {
      import Tree._
      import cats.syntax.functor._

      branch(leaf(3), branch(leaf(1), leaf(2))).map(_ * 2) shouldEqual
        branch(leaf(6), branch(leaf(2), leaf(4)))
    }
  }
}
