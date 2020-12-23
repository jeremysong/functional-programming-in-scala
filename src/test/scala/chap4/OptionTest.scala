package chap4

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
}
