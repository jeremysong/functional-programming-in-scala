package chap4

sealed trait Either[+E, +A] {
  /** Exercise 4.6 */
  self =>

  def map[B](f: A => B): Either[E, B] = self match {
    case Right(v) =>
      try Right(f(v))
      catch {
        case e: E => Left(e)
      }
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = self match {
    case Right(v) => f(v)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = self match {
    case Left(_) => b
    case _ => self
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    flatMap(aa => {
      b.map(bb => f(aa, bb))
    })
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  /** Exercise 4.7 */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case Left(e) :: _ => Left(e)
    case Right(head) :: tail => sequence(tail) map (head :: _)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = sequence(as.map(f(_)))
}