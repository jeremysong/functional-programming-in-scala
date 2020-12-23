package chap4

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

  /** Exercise 4.2 */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => if (xs.isEmpty) None else Some(xs.map(x => Math.pow(x - m, 2)).sum / xs.length))
  }
}