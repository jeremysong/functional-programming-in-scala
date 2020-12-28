package chap5

import chap5.Stream.constant
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
    Stream(-1, 0, 1, 2).takeWhile(isPositive) shouldEqual Empty
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
    Stream(-1, 0, 1, 2).takeWhile2(isPositive) shouldEqual Empty
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

  it should "constant" in {
    val as = constant(1)
    as.take(5).toList shouldEqual List(1, 1, 1, 1, 1)
  }

  it should "from" in {
    Stream.from(1).take(5).toList shouldEqual List(1, 2, 3, 4, 5)
  }

  it should "fibs" in {
    Stream.fibs().take(7).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8)
  }

  it should "unfold" in {
    Stream.unfold(0)(s => Some(s + 1, s + 1)).take(3).toList shouldEqual List(1, 2, 3)
    Stream.unfold(0)(_ => None) shouldEqual Empty
  }

  it should "unfoldFibs" in {
    Stream.unfoldFibs().take(7).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8)
  }

  it should "unfoldFrom" in {
    Stream.unfoldFrom(1).take(5).toList shouldEqual List(1, 2, 3, 4, 5)
  }

  it should "unfoldConstant" in {
    Stream.unfoldConstant(1).take(5).toList shouldEqual List(1, 1, 1, 1, 1)
  }

  it should "unfoldOnes" in {
    Stream.unfoldOnes().take(5).toList shouldEqual List(1, 1, 1, 1, 1)
  }

  it should "unfoldMap" in {
    val addOne = (a: Int) => a + 1
    Empty.unfoldMap(addOne) shouldEqual Empty
    Stream(1, 2).unfoldMap(addOne).toList shouldEqual Stream(2, 3).toList
  }

  it should "unfoldTake" in {
    Empty.unfoldTake(0) shouldEqual Empty
    Empty.unfoldTake(2) shouldEqual Empty
    Stream(1, 2).unfoldTake(0) shouldEqual Empty
    Stream(1, 2).unfoldTake(3).toList shouldEqual List(1, 2)
    Stream(1, 2).unfoldTake(1).toList shouldEqual List(1)
  }

  it should "unfoldTakeWhile" in {
    val isPositive = (x: Int) => x > 0
    Empty.unfoldTakeWhile(isPositive) shouldEqual Empty
    Stream(1, 2).unfoldTakeWhile(isPositive).toList shouldEqual List(1, 2)
    Stream(-1, 0, 1, 2).unfoldTakeWhile(isPositive) shouldEqual Empty
    Stream(-1, 0).unfoldTakeWhile(isPositive) shouldEqual Empty
  }

  it should "unfoldZipAll" in {
    Empty.zipAll(Empty) shouldEqual Empty
    Empty.zipAll(Stream(1, 2)).toList shouldEqual List((None, Some(1)), (None, Some(2)))
    Stream(1, 2).zipAll(Stream(3, 4)).toList shouldEqual List((Some(1), Some(3)), (Some(2), Some(4)))
    Stream(1, 2).zipAll(Empty).toList shouldEqual List((Some(1), None), (Some(2), None))
    Stream(1, 2).zipAll(Stream(3, 4, 5)).toList shouldEqual
      List((Some(1), Some(3)), (Some(2), Some(4)), (None, Some(5)))
  }

  it should "startsWith" in {
    Empty.startsWith(Empty) shouldBe false
    Empty.startsWith(Stream(1, 2)) shouldBe false
    Stream(1, 2).startsWith(Empty) shouldBe false
    Stream(1, 2, 3).startsWith(Stream(1, 2)) shouldBe true
    Stream(1, 2).startsWith(Stream(1, 2, 3)) shouldBe false
  }

  it should "tails" in {
    Empty.tails.toList shouldEqual List(Empty)
    Stream(1, 2, 3).tails.toList.map(_.toList) shouldEqual List(List(1, 2, 3), List(2, 3), List(3), List())
  }

  it should "scanRight" in {
    Stream.empty[Int].scanRight(0)(_ + _).toList shouldEqual List(0)
    Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldEqual List(6, 5, 3, 0)
  }
}
