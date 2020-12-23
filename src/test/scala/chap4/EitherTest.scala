package chap4

import chap4.Either.{sequence, traverse}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.a
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class EitherTest extends AnyFlatSpec {

  behavior of "EitherTest"

  it should "map" in {
    val toInt: String => Int = (s: String) => s.toInt
    Left("left").map(toInt) shouldEqual Left("left")
    Right("1").map(toInt) shouldEqual Right(1)
    Right("right").map(toInt) shouldBe a[Left[NumberFormatException]]
  }

  it should "map2" in {
    val concat: (String, String) => String = (a, b) => a.concat(b)
    Left("a").map2(Left("b"))(concat) shouldEqual Left("a")
    Left("a").map2(Right("b"))(concat) shouldEqual Left("a")
    Right("a").map2(Left("b"))(concat) shouldEqual Left("b")
    Right("a").map2(Right("b"))(concat) shouldEqual Right("ab")
  }

  it should "orElse" in {
    Left("a").orElse(Right("b")) shouldEqual Right("b")
    Right("a").orElse(Right("b")) shouldEqual Right("a")
  }

  it should "flatMap" in {
    val toInt: String => Either[Exception, Int] = (s: String) =>
      try Right(s.toInt)
      catch {
        case e: Exception => Left(e)
      }

    Left("left").flatMap(toInt) shouldEqual Left("left")
    Right("1").flatMap(toInt) shouldEqual Right(1)
    Right("right").flatMap(toInt) shouldBe a[Left[NumberFormatException]]
  }

  it should "sequence" in {
    sequence(Nil) shouldEqual Right(Nil)
    sequence(Right(1) :: Right(2) :: Nil) shouldEqual Right(1 :: 2 :: Nil)
    sequence(Right(1) :: Left(2) :: Left(3) :: Nil) shouldEqual Left(2)
  }

  it should "traverse" in {
    val toInt: String => Either[Exception, Int] = (s: String) =>
      try Right(s.toInt)
      catch {
        case e: Exception => Left(e)
      }

    traverse(Nil)(toInt) shouldEqual Right(Nil)
    traverse("1" :: "2" :: Nil)(toInt) shouldEqual Right(1 :: 2 :: Nil)
    traverse("1" :: "a" :: Nil)(toInt) shouldBe a[Left[NumberFormatException]]
  }
}
