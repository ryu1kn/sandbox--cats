package sandbox.part1

import cats.Eval
import sandbox.helper.Specification

class EvalTest extends Specification {

  "Eval" should {
    def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
      case head :: tail => Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil => acc
    }

    def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
      foldRightEval(as, Eval.now(acc)) { (a, b) => b.map(fn(a, _)) }.value

    "be used to make foldRight stack-safe" in {
      foldRight((1 to 100000).toList, 0L)(_ + _) shouldEqual 5000050000L
    }
  }
}
