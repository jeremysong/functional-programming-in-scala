package chap4

import scala.annotation.tailrec

trait Option[+A] {
  /** Exercise 4.1 */
  self =>

  def map[B](f: A => B): Option[B] = self match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = self match {
    case Some(a) => f(a)
    case _ => None
  }

  def getOrElse[B >: A](default: => B): B = self match {
    case Some(a) => a
    case _ => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = self match {
    case Some(_) => self
    case _ => ob
  }

  def filter(f: A => Boolean): Option[A] = self match {
    case Some(a) => if (!f(a)) None else Some(a)
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {
      case e: Exception => None
    }
  }

  /** Exercise 4.2 */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => if (xs.isEmpty) None else Some(xs.map(x => Math.pow(x - m, 2)).sum / xs.length))
  }

  /** Exercise 4.3 */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case _ => None
  }

  /** Exercise 4.4 */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case None :: _ => None
    case Some(x) :: xs => sequence(xs) map (xs => x :: xs)
  }

  /** Exercise 4.5 */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => f(x).flatMap(r => traverse(xs)(f) match {
      case Some(xs) => Some(r :: xs)
      case _ => None
    })
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)
}