package chap3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  /**
   * Exercise 3.2
   */
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  /**
   * Exercise 3.3
   */
  def setHead[A](head: A, as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(head, xs)
  }

  /**
   * Exercise 3.4
   */
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l else l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }
  }

  /**
   * Exercise 3.5
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) xs else Cons(x, dropWhile(xs, f))
  }

  /**
   * Exercise 3.6
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  /** Exercise 3.9 */
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, y) => 1 + y)

  /** Exercise 3.10 */
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def length2[A](as: List[A]): Int = foldLeft(as, 0)((x, _) => x + 1)

  /** Exercise 3.11 */
  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  /** Exercise 3.12 */
  def reverse[A](ns: List[A]): List[A] = ns match {
    case Nil => ns
    case Cons(x, xs) => foldLeft(xs, List(x))((l, x) => Cons(x, l))
  }

  /** Exercise 3.13 */
  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, z)((a, b) => f(b, a))
  }

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, z)((a, b) => f(b, a))
  }

  /** Exercise 3.14 */
  def append[A](as: List[A], a: A): List[A] = {
    foldRight(as, List(a))((b, l) => Cons(b, l))
  }

  /** Exercise 3.15 */
  def concat[A](as: List[List[A]]): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => foldLeft(xs, x)((l1, l2) => foldRight(l1, l2)((a, b) => Cons(a, b)))
  }
}