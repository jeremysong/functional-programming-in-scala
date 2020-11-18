package chap3

import org.scalatest.flatspec.AnyFlatSpec
import chap3.List._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ListTest extends AnyFlatSpec {
  behavior of "List"

  /**
    * Exercise 3.1
    */
  it should "return 3" in {
    val x = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
    }

    x shouldEqual 3
  }

  it should "return correct tails" in {
    tail(List(1, 2, 3)) shouldEqual List(2, 3)
    tail(Nil) shouldEqual Nil
  }

  it should "set correct head" in {
    setHead(1, List(2, 3)) shouldEqual List(1, 3)
    tail(Nil) shouldEqual Nil
  }

  it should "drop correct items" in {
    drop(Nil, 1) shouldEqual Nil
    drop(List(1, 2, 3), 0) shouldEqual List(1, 2, 3)
    drop(List(1, 2, 3), 1) shouldEqual List(2, 3)
    drop(List(1, 2, 3), 4) shouldEqual Nil
  }
}