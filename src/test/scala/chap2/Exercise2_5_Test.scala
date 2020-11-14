package chap2

import chap2.Exercise2_5.compose
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Exercise2_5_Test extends AnyFlatSpec {

  behavior of "Exercise2_5_Test"

  it should "compose" in {
    val add2 = compose((b : Int) => b + 1, (a: Int) => a + 1)
    add2(3) shouldEqual 5
  }
}
