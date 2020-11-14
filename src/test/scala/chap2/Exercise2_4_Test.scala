package chap2

import chap2.Exercise2_4.uncurry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Exercise2_4_Test extends AnyFlatSpec {

  behavior of "Exercise2_4_Test"

  it should "uncurry" in {
    val add = uncurry((a: Int) => (b: Int) => a + b)
    add(1, 2) shouldEqual 3
  }
}
