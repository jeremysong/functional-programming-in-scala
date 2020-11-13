package chap2

object Exercise2_1 {
  def fib(n: Int): Int = {
    n match {
      case _ if n < 0 => throw new IllegalArgumentException(s"$n has to be positive")
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 1) + fib(n - 2)
    }
  }
}
