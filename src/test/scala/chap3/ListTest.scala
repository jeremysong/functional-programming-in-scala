package chap3

import org.scalatest.flatspec.AnyFlatSpec
import chap3.List.{length, _}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.annotation.tailrec

class ListTest extends AnyFlatSpec {
  behavior of "List"

  /**
   * Exercise 3.1
   */
  it should "return 3" in {
    val x = List(1, 2, 3, 4, 5) match {
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

  it should "dropWhile correct items" in {
    val predicate = (x: Int) => x == 3
    dropWhile(Nil, predicate) shouldEqual Nil
    dropWhile(List(1, 2, 3), predicate) shouldEqual List(1, 2)
    dropWhile(List(1, 2, 4), predicate) shouldEqual List(1, 2, 4)
  }

  it should "init correct items" in {
    init(Nil) shouldEqual Nil
    init(List(1, 2, 3)) shouldEqual List(1, 2)
  }

  it should "calculate correct length using foldRight" in {
    length(Nil) shouldEqual 0
    length(List(1)) shouldEqual 1
    length(List(1, 2, 3)) shouldEqual 3
  }

  it should "calculate correct length using foldLeft" in {
    length2(Nil) shouldEqual 0
    length2(List(1)) shouldEqual 1
    length2(List(1, 2, 3)) shouldEqual 3
  }

  it should "calculate correct results using sum3 and product3" in {
    sum3(Nil) shouldEqual 0
    sum3(List(1, 2)) shouldEqual 3

    product3(Nil) shouldEqual 1.0
    product3(List(1, 3)) shouldEqual 3.0
  }

  it should "reverse a list" in {
    reverse(Nil) shouldEqual Nil
    reverse(List(1, 2)) shouldEqual List(2, 1)
    reverse(List(1, 2, 3)) shouldEqual List(3, 2, 1)
  }

  it should "calculate correct length using foldLeft2" in {
    def length[A](as: List[A]): Int = foldLeft2(as, 0)((x, _) => x + 1)

    length(Nil) shouldEqual 0
    length(List(1)) shouldEqual 1
    length(List(1, 2, 3)) shouldEqual 3
  }

  it should "calculate correct length using foldRight2" in {
    def length[A](as: List[A]): Int = foldRight2(as, 0)((_, y) => y + 1)

    length(Nil) shouldEqual 0
    length(List(1)) shouldEqual 1
    length(List(1, 2, 3)) shouldEqual 3
  }

  it should "append correct item to list" in {
    append(Nil, 1) shouldEqual List(1)
    append(List(1, 2), 3) shouldEqual List(1, 2, 3)
  }

  it should "concat two list correctly" in {
    concat(Nil) shouldEqual Nil
    concat(List(List(1), List(2))) shouldEqual List(1, 2)
    concat(List(List(1, 2), List(3, 4), List(5, 6))) shouldEqual List(1, 2, 3, 4, 5, 6)
  }

  /** Exercise 3.16 */
  it should "add 1 to each element in the list" in {
    addOne(Nil) shouldEqual Nil
    addOne(List(1, 2)) shouldEqual List(2, 3)
  }

  it should "turns each value to string" in {
    toStringList(Nil) shouldEqual Nil
    toStringList(List(1.0, 2.0)) shouldEqual List("1.0", "2.0")
  }

  it should "map each value in the list" in {
    map(Nil)((a: Nothing) => a) shouldEqual Nil
    map(List(1, 2, 3))(_.toString) shouldEqual List("1", "2", "3")
  }

  it should "filter out odd number" in {
    filter(Nil)((a: Nothing) => a) shouldEqual Nil
    filter(List(1, 2, 3))(_ % 2 == 0) shouldEqual List(1, 3)
  }

  it should "flatmap the list" in {
    flatMap(Nil)((i: Nothing) => List(i, i)) shouldEqual Nil
    flatMap(List(1, 2, 3))(i => List(i, i)) shouldEqual List(1, 1, 2, 2, 3, 3)
  }

  it should "filter using flatmap" in {
    filter2(Nil)((a: Nothing) => a) shouldEqual Nil
    filter2(List(1, 2, 3))(_ % 2 == 0) shouldEqual List(1, 3)
  }

  it should "calculate correct pair-wise sum" in {
    pairWiseSum(Nil, Nil) shouldEqual Nil
    pairWiseSum(List(1, 2, 3), List(4, 5, 6)) shouldEqual List(5, 7, 9)
  }

  it should "zipWith with sum" in {
    zipWith(Nil, Nil)((a: Nothing, _: Nothing) => a) shouldEqual Nil
    zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) shouldEqual List(5, 7, 9)
  }

  it should "check correct sub list" in {
    hasSubsequence(Nil, List(1)) shouldEqual false
    hasSubsequence(List(1), Nil) shouldEqual false
    hasSubsequence(List(1), List(1)) shouldEqual true
    hasSubsequence(List(1, 2), List(1)) shouldEqual true
    hasSubsequence(List(1, 2, 3, 4), List(2, 3)) shouldEqual true
    hasSubsequence(List(1, 2, 3, 4), List(3, 2)) shouldEqual false
    hasSubsequence(List(1, 2, 3, 4, 5, 6), List(2, 3)) shouldEqual true
    hasSubsequence(List(1, 2, 3, 4, 5, 6), List(3, 2)) shouldEqual false
    hasSubsequence(List(1, 2, 3, 4, 5, 6), List(3, 5)) shouldEqual false
  }
}