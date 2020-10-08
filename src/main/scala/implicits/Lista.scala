/**
* Author: Elio Ventocilla
*/
package implicits

sealed abstract class Lista[+A] {
    def head: A
    def tail: Lista[A]
    def :: [B >: A](e: B): Lista[B]
    def + [B >: A](other: Lista[B]): Lista[B]
    def isSorted[B >: A](implicit order: Ordering[B]): Boolean
}

case class Cons[A](head: A, tail: Lista[A]) extends Lista[A] {
    def :: [B >: A](e: B): Lista[B] = Cons(e, this)

    def + [B >: A](other: Lista[B]): Lista[B] =
        Cons(head, tail + other)

    def isSorted[B >: A](implicit order: Ordering[B]): Boolean = tail match {
      case Cons(h, _) => order.gteq(h, head) && tail.isSorted(order)
      case _ => true
    }
}

case object Empty extends Lista[Nothing] {
  def head = throw new Exception("Empty has no head")
  def tail = throw new Exception("Empty has no tail")

  def :: [A](e: A): Lista[A] = Cons(e, this)
  def + [B](other: Lista[B]): Lista[B] = other
  def isSorted[B](implicit order: Ordering[B]): Boolean = true
}

object Lista {
  def apply[A](ns: A*): Lista[A] = {
    if (ns.isEmpty) Empty
    else Cons(ns.head, apply(ns.tail:_*))
  }
}
