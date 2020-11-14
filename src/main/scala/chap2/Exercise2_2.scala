package chap2

import scala.annotation.tailrec

object Exercise2_2 {
  @tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as.length match {
      case 0 | 1 => true
      case _ => ordered(as(0), as(1)) && isSorted(as.tail, ordered)
    }
  }
}
