package chap6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  /** Exercise 6.1 */
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (n, nextRNG) if n >= 0 => (n, nextRNG)
    case (n, nextRNG) => (-(n + 1), nextRNG)
  }

  /** Exercise 6.4 */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case count if count <= 0 => (Nil, rng)
    case count =>
      val (n, r1) = rng.nextInt
      val (pre, r2) = ints(count - 1)(r1)
      (n :: pre, r2)
  }

  /** Exercise 6.5 */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs match {
        case Nil => (Nil, rng)
        case x :: xs => {
          val (r, rng2) = x(rng)
          val (rs, rng3) = sequence(xs)(rng2)
          (r :: rs, rng3)
        }
      }
    }

  def sequenceInts(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(_.nextInt))
}
