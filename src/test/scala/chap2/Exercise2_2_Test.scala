package chap2

import chap2.Exercise2_2.isSorted
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Exercise2_2_Test extends AnyFlatSpec {

  val compareInt: (Int, Int) => Boolean = (a: Int, b: Int) => a <= b
  val compareDouble: (Double, Double) => Boolean = (a: Double, b: Double) => a <= b
  
  behavior of "isSorted"

  it should "treat empty array as sorted" in {
    isSorted(Array(), compareInt) shouldEqual true
  }
  
  it should "treat singleton array as sorted" in {
    isSorted(Array(1), compareInt) shouldEqual true
  }
  
  it should "return true for sorted array" in {
    isSorted(Array(1, 2, 5, 10), compareInt) shouldEqual true
  }
  
  it should "return false for un-sorted array" in {
    isSorted(Array(1, 5, 2, 10), compareInt) shouldEqual false
  }
  
  it should "return true for array with same elements" in {
    isSorted(Array(1, 1, 1, 1), compareInt) shouldEqual true
  }
  
  it should "be able to test double type" in {
    isSorted(Array(1.2D, 2.3D, 2.5D, 10D), compareDouble) shouldEqual true
  }
}
