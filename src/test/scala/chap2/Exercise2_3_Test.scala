package chap2

import chap2.Exercise2_3.curry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Exercise2_3_Test extends AnyFlatSpec {

  behavior of "Exercise2_3_Test"

  it should "curry" in {
    val add = curry((a: Int, b: Int) => a + b)
    add(1)(2) shouldEqual 3
  }
}
