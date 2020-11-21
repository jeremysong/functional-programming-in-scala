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

  /** Exercise 3.16 */
  def addOne(is: List[Int]): List[Int] = is match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  /** Exercise 3.17 */
  def toStringList(ds: List[Double]): List[String] = ds match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, toStringList(xs))
  }

  /** Exercise 3.18 */
  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  /** Exercise 3.19 */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) filter(xs)(f) else Cons(x, filter(xs)(f))
  }

  /** Exercise 3.20 */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => foldRight(f(x), flatMap(xs)(f))((a, b) => Cons(a, b))
  }

  /** Exercise 3.21 */
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as) {
      case a if f(a) => Nil
      case a => List(a)
    }
  }

  /** Exercise 3.22 */
  def pairWiseSum(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) | (_, Nil) | (Nil, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(a + b, pairWiseSum(as, bs))
  }

  /** Exercise 3.23 */
  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = (as, bs) match {
    case (Nil, _) | (_, Nil) | (Nil, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
  }

  /** Exercise 3.24 */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def suffixCompare(sup: List[A], sub:List[A]): Boolean = sup match {
      case Nil => false
      case _ if sup == sub => true
      case Cons(x, xs) => suffixCompare(xs, sub) | suffixCompare(reverse(xs), reverse(sub))
    }

    suffixCompare(sup, sub) | suffixCompare(reverse(sup), reverse(sub))
  }
}