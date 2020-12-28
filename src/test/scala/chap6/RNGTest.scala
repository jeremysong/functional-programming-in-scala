package chap6

import chap6.RNG.nonNegativeInt
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class RNGTest extends AnyFlatSpec {

  behavior of "RNGTest"

  it should "nonNegativeInt" in {
    RNG.nonNegativeInt(SimpleRNG(1))._1 should be >= 0
  }

  it should "ints" in {
    val (list, _) = RNG.ints(3)(SimpleRNG(1))
    print(list)
    list.length shouldEqual 3
  }

  it should "map2" in {
    println(RNG.map2(nonNegativeInt, nonNegativeInt)(_ + _)(SimpleRNG(1))._1)
  }

  it should "sequenceInt" in {
    val (list, _) = RNG.sequenceInts(3)(SimpleRNG(1))
    println(list)
    list.length shouldEqual 3
  }
}
