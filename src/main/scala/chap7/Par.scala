package chap7

class Par[A] {
  def unit(a: => A): Par[A] = ???

  def get(a: Par[A]): A = ???
}

object Par {
  /** Exercise 7.1 */
  def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = ???
}
