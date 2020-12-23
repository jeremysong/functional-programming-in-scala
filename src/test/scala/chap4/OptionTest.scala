package chap4

import chap4.Option.{Try, map2, sequence, sequence2, traverse}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class OptionTest extends AnyFlatSpec {

  behavior of "OptionTest"

  it should "orElse" in {
    Some("string").orElse(Some("anotherString")) shouldEqual Some("string")
    None.orElse(Some("anotherString")) shouldEqual Some("anotherString")
  }

  it should "filter" in {
    Some("string").filter(s => s.contains("s")) shouldEqual Some("string")
    Some("string").filter(s => s.contains("a")) shouldEqual None
    None.filter(_ => true) shouldEqual None
  }

  it should "map" in {
    Some("string").map(s => s.toUpperCase) shouldEqual Some("STRING")
    None.map((s: String) => s.toUpperCase()) shouldEqual None
  }

  it should "flatMap" in {
    Some("string").flatMap(s => Some(s.toUpperCase)) shouldEqual Some("STRING")
    None.flatMap((s: String) => Some(s.toUpperCase)) shouldEqual None
  }

  it should "getOrElse" in {
    Some("string").getOrElse("anotherString") shouldEqual "string"
    None.getOrElse("string") shouldEqual "string"
  }

  it should "variance" in {
    Option.variance(Nil) shouldEqual None
    Option.variance(1D :: 1D :: 1D :: Nil) shouldEqual Some(0D)
  }

  it should "map2" in {
    val add = (a: Int, b: Int) => a + b
    map2(None, None)(add) shouldEqual None
    map2(None, Some(2))(add) shouldEqual None
    map2(Some(1), None)(add) shouldEqual None
    map2(Some(1), Some(2))(add) shouldEqual Some(3)
  }

  it should "sequence" in {
    sequence(Nil) shouldEqual Some(Nil)
    sequence(Some(1) :: Some(2) :: Nil) shouldEqual Some(1 :: 2 :: Nil)
    sequence(Some(1) :: None :: Nil) shouldEqual None
  }

  it should "traverse" in {
    val toInt = (s: String) => Try(s.toInt)
    traverse(Nil)(toInt) shouldEqual Some(Nil)
    traverse("1" :: "2" :: Nil)(toInt) shouldEqual Some(1 :: 2 :: Nil)
    traverse("1" :: "a" :: Nil)(toInt) shouldEqual None
  }

  it should "sequence2" in {
    sequence2(Nil) shouldEqual Some(Nil)
    sequence2(Some(1) :: Some(2) :: Nil) shouldEqual Some(1 :: 2 :: Nil)
    sequence2(Some(1) :: None :: Nil) shouldEqual None
  }
}
