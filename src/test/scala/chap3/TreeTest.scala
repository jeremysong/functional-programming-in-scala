package chap3

import chap3.Tree._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class TreeTest extends org.scalatest.flatspec.AnyFlatSpec {
  behavior of "Tree"

  it should "return correct number of leaves" in {
    sum(Leaf(1)) shouldEqual 1
    sum(Branch(Leaf(1), Leaf(2))) shouldEqual 2
    sum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) shouldEqual 4
  }

  it should "return correct max value" in {
    maximum(Leaf(1)) shouldEqual 1
    maximum(Branch(Leaf(1), Leaf(2))) shouldEqual 2
    maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) shouldEqual 4
  }

  it should "return correct depth of the tree" in {
    depth(Leaf(1)) shouldEqual 1
    depth(Branch(Leaf(1), Leaf(2))) shouldEqual 2
    depth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))) shouldEqual 4
  }

  it should "return correct tree after map" in {
    map(Leaf(1))(_ + 1) shouldEqual Leaf(2)
    map(Branch(Leaf(1), Leaf(2)))(_ + 1) shouldEqual Branch(Leaf(2), Leaf(3))
  }

  it should "return correct depth with fold" in {
    fold(Leaf(1))(_ => 1)((l, r) => (l + 1) max (r + 1)) shouldEqual 1
    fold(Branch(Leaf(1), Leaf(2)))(_ => 1)((l, r) => (l + 1) max (r + 1)) shouldEqual 2

    (fold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5))))
      (_ => 1)((l, r) => (l + 1) max (r + 1)) shouldEqual 4)
  }

}
