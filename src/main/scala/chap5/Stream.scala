package chap5

import chap5.Stream.{cons, empty, unfold}

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /** Exercise 5.1 */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /** Exercise 5.2 */
  def take(n: Int): Stream[A] = (this, n) match {
    case (_, n) if n <= 0 => Empty
    case (Empty, _) => Empty
    case (Cons(h, t), n) => cons(h(), t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = (this, n) match {
    case (_, n) if n <= 0 => this
    case (Empty, _) => this
    case (Cons(_, t), n) => t().drop(n - 1)
  }

  /** Exercise 5.3 */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  /** Exercise 5.4 */
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => false
    case Cons(h, t) if t() == Empty => p(h())
    case Cons(h, t) => p(h()) && t().forAll(p)
  }

  /** Exercise 5.5 */
  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)
  }

  /** Exercise 5.6 */
  def headOption2: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }

  /** Exercise 5.7 */
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a, b) => cons(f(a), b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)
  }

  def append[B >: A](z: => B): Stream[B] = {
    foldRight(Stream(z))((a, b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a, b) => f(a).foldRight(b)((a, b) => cons(a, b)))
  }

  /** Exercise 5.13 */
  def unfoldMap[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }
  }

  def unfoldTake(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (s, n) if n <= 0 || s == Empty => None
      case (Cons(h, t), n) => Some(h(), (t(), n - 1))
    }
  }

  def unfoldTakeWhile(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold(this, s2) {
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case _ => None
    }
  }

  /** Exercise 5.14 */
  def startsWith[B >: A](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h, h2) => h == h2
    }
  }

  /** Exercise 5.15 */
  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some(cons(h(), t()), t())
    } append empty
  }

  /** Exercise 5.16 */
  def scanRight[B](z: => B)(f: (A, B) => B): Stream[B] = {
    foldRight((Stream(z), z))((a, b) => {
      val result = f(a, b._2)
      (cons(result, b._1), result)
    })._1
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /** Exercise 5.8 */
  def constant[A](a: A): Stream[A] = {
    lazy val as: Stream[A] = Cons(() => a, () => as)
    as
  }

  /** Exercise 5.9 */
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  /** Exercise 5.10 */
  def fibs(): Stream[Int] = {
    def fib(n: Int): Int = n match {
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 1) + fib(n - 2)
    }
    from(0).map(fib)
  }

  /** Exercise 5.11 */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }

  /** Exercise 5.12 */
  def unfoldFibs(): Stream[Int] = {
    def fib(n: Int): Int = n match {
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 1) + fib(n - 2)
    }

    unfold(0)(s => Some(fib(s), s + 1))
  }

  def unfoldFrom(n: Int): Stream[Int] = {
    unfold(n)(s => Some(s, s + 1))
  }

  def unfoldConstant(n: Int): Stream[Int] = {
    unfold(n)(s => Some(s, s))
  }

  def unfoldOnes(): Stream[Int] = {
    unfold(1)(_ => Some(1, 1))
  }
}
