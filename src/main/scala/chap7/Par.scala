package chap7

import java.util.concurrent.{ExecutorService, Future, TimeUnit}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  private case class TimedFuture[A, B, C](af: Future[A], bf: Future[B], f: (A, B) => C) extends Future[C] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = af.cancel(mayInterruptIfRunning) || bf.cancel(mayInterruptIfRunning)

    override def isCancelled: Boolean = af.isCancelled || bf.isCancelled

    override def isDone: Boolean = af.isDone && bf.isDone

    override def get(timeout: Long, unit: TimeUnit): C = {
      val timeoutInNs = TimeUnit.NANOSECONDS.convert(timeout, unit)
      val start = System.currentTimeMillis()
      val ar = af.get
      val end = System.currentTimeMillis()
      val br = bf.get(timeoutInNs - (end - start), TimeUnit.NANOSECONDS)
      f(ar, br)
    }

    override def get(): C = get(Long.MaxValue, TimeUnit.NANOSECONDS)
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def fork[A](a: => Par[A]): Par[A] = {
    es => es.submit(() => a(es).get)
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /** Exercise 7.1 */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  /** Exercise 7.3 */
  def timedMap2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      TimedFuture(a(es), b(es), f)
    }
  }

  /** Exercise 7.4 */
  def asyncF[A, B](f: A => B): A => Par[B] = {
    (a: A) => {
      lazyUnit(f(a))
    }
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /** Exercise 7.5 */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight[Par[List[A]]](unit(Nil))((h, t) => map2(h, t)(_ :: _))
  }

  /** Exercise 7.6 */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fbs: List[Par[List[A]]] = as.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(fbs))(_.flatten)
  }
}
