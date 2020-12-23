package chap5

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /** Exercise 5.1 */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /** Exercise 5.2 */
  def take(n: Int): List[A] = (this, n) match {
    case (_, n) if n <= 0 => Nil
    case (Empty, _) => Nil
    case (Cons(h, t), n) => h() :: t().take(n - 1)
  }

  def drop(n: Int): Stream[A] = (this, n) match {
    case (_, n) if n <= 0 => this
    case (Empty, _) => this
    case (Cons(_, t), n) => t().drop(n - 1)
  }

  /** Exercise 5.3 */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case Cons(_, t) => t().takeWhile(p)
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
