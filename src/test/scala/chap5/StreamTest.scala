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
    Empty.take(0) shouldEqual Empty
    Empty.take(2) shouldEqual Empty
    Stream(1, 2).take(0) shouldEqual Empty
    Stream(1, 2).take(3).toList shouldEqual List(1, 2)
    Stream(1, 2).take(1).toList shouldEqual List(1)
  }

  it should "takeWhile" in {
    val isPositive = (x: Int) => x > 0
    Empty.takeWhile(isPositive) shouldEqual Empty
    Stream(1, 2).takeWhile(isPositive).toList shouldEqual List(1, 2)
    Stream(-1, 0, 1, 2).takeWhile(isPositive).toList shouldEqual List(1, 2)
    Stream(-1, 0).takeWhile(isPositive) shouldEqual Empty
  }

  it should "forAll" in {
    val isPositive = (x: Int) => x > 0
    Empty.forAll(isPositive) shouldEqual false
    Stream(1).forAll(isPositive) shouldEqual true
    Stream(-1).forAll(isPositive) shouldEqual false
    Stream(1, 2, 3).forAll(isPositive) shouldEqual true
    Stream(-1, 2, 3).forAll(isPositive) shouldEqual false
  }

  it should "takeWhile2" in {
    val isPositive = (x: Int) => x > 0
    Empty.takeWhile2(isPositive) shouldEqual Empty
    Stream(1, 2).takeWhile2(isPositive).toList shouldEqual List(1, 2)
    Stream(-1, 0, 1, 2).takeWhile2(isPositive).toList shouldEqual List(1, 2)
    Stream(-1, 0).takeWhile2(isPositive) shouldEqual Empty
  }

  it should "headOption2" in {
    Empty.headOption2 shouldEqual None
    Stream(1, 2).headOption2 shouldEqual Some(1)
    Stream(1).headOption2 shouldEqual Some(1)
  }

  it should "map" in {
    val addOne = (a: Int) => a + 1
    Empty.map(addOne) shouldEqual Empty
    Stream(1, 2).map(addOne).toList shouldEqual Stream(2, 3).toList
  }

  it should "filter" in {
    val isPositive = (x: Int) => x > 0
    Empty.filter(isPositive) shouldEqual Empty
    Stream(1, 2).filter(isPositive).toList shouldEqual List(1, 2)
    Stream(-1, 0, 1, 2).filter(isPositive).toList shouldEqual List(1, 2)
    Stream(-1, 0).filter(isPositive) shouldEqual Empty
  }

  it should "append" in {
    Stream.empty[Int].append(1).toList shouldEqual Stream(1).toList
    Stream(1, 2).append(3).toList shouldEqual Stream(1, 2, 3).toList
  }

  it should "flatMap" in {
    val toStream = (a: Int) => Stream(a, a)
    Stream.empty[Int].flatMap(toStream) shouldEqual Empty
    Stream(1, 2).flatMap(toStream).toList shouldEqual Stream(1, 1, 2, 2).toList
  }
}
