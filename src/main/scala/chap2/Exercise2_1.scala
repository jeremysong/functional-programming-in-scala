package chap2

import scala.annotation.tailrec

object Exercise2_1 {
  def fib(n: Int): Int = {
    if (n < 0) throw new IllegalArgumentException

    @tailrec
    def go(m: Int, a: Int = 0, b: Int = 1): Int = m match {
      case 0 => a
      case 1 => b
      case _ => go(m - 1, b, a + b)
    }

    go(n)
  }
}
