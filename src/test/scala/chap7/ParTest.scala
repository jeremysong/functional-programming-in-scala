package chap7

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import java.util.concurrent.Executors

class ParTest extends AnyFlatSpec {

  behavior of "ParTest"

  it should "asyncF" in {
    Par.asyncF((a: Int) => a * 2)(2)(Executors.newCachedThreadPool()).get() shouldBe 4
  }

  it should "sequence" in {
    Par.sequence(Par.lazyUnit(1) :: Par.lazyUnit(2) :: Nil)(Executors.newCachedThreadPool()).get() shouldBe List(1, 2)
  }

  it should "partFilter" in {
    Par.parFilter(-1 :: 2 :: Nil)(_ > 0)(Executors.newCachedThreadPool()).get shouldBe List(2)
  }
}
