package sandbox.part1

import sandbox.part1.helper.Specification

class MonadTreeTest extends Specification {
  sealed trait Tree[+A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  import cats.Monad

  implicit val treeMonad = new Monad[Tree] {
    override def pure[A](value: A): Tree[A] = leaf(value)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(left, right) => branch(flatMap(left)(f), flatMap(right)(f))
      case Leaf(value) => f(value)
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(f(a)) {
        case Left(value) => tailRecM(value)(f)
        case Right(value) => Leaf(value)
      }
  }

  "Tree" should {
    "become a monad" in {
      import cats.syntax.flatMap._

      branch(leaf(3), leaf(5)).flatMap(x => branch(leaf(x - 1), leaf(x + 1))) shouldEqual
        branch(branch(leaf(2), leaf(4)), branch(leaf(4), leaf(6)))
    }
  }
}
