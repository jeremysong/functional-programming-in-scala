package chap5

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class StreamTest extends AnyFlatSpec {

  behavior of "StreamTest"

  it should "toList" in {
    Empty.toList shouldEqual Nil
    Stream(1, 2, 3).toList shouldEqual 1 :: 2 :: 3 :: Nil
  }

  it should "drop" in {
    Empty.drop(0) shouldEqual Empty
    Empty.drop(2) shouldEqual Empty
    Stream(1, 2).drop(0).toList shouldEqual List(1, 2)
    Stream(1, 2).drop(3) shouldEqual Empty
    Stream(1, 2).drop(1).toList shouldEqual List(2)
  }

  it should "take" in {
    Empty.take(0) shouldEqual Nil
    Empty.take(2) shouldEqual Nil
    Stream(1, 2).take(0) shouldEqual Nil
    Stream(1, 2).take(3) shouldEqual List(1, 2)
    Stream(1, 2).take(1) shouldEqual List(1)
  }

  it should "takeWhile" in {
    val isPositive = (x: Int) => x > 0
    Empty.takeWhile(isPositive) shouldEqual Empty
    Stream(1, 2).takeWhile(isPositive).toList shouldEqual List(1, 2)
    Stream(-1, 0, 1, 2).takeWhile(isPositive).toList shouldEqual List(1, 2)
    Stream(-1, 0).takeWhile(isPositive) shouldEqual Empty
  }
}
