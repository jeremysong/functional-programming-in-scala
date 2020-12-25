package chap5

import chap5.Stream.{cons, empty}

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
    case Empty => Empty
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case Cons(_, t) => t().takeWhile(p)
  }

  /** Exercise 5.4 */
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => false
    case Cons(h, t) if t() == Empty => p(h())
    case Cons(h, t) => p(h()) && t().forAll(p)
  }

  /** Exercise 5.5 */
  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)
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
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
