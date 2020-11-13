package chap2

import chap2.Exercise2_1._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.contain
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Exercise2_1_Test extends AnyFlatSpec {

  behavior of "fib"

  it should "output 0 when given 0" in {
    fib(0) shouldEqual 0
  }

  it should "output 1 when given 1" in {
    fib(1) shouldEqual 1
  }

  it should "return correct sequence" in {
    0 to 8 map (i => fib(i)) should contain theSameElementsInOrderAs 0 :: 1 :: 1 :: 2 :: 3 :: 5 :: 8 :: 13 :: 21 :: Nil
  }
}
