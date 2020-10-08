/**
* Author: Elio Ventocilla
*/
package contravariance

/**
* Another implementation of Lista (covariant), where the "map" function takes a
* Callable object (see Callable.scala) instead of a Scala function.
*
* @param A The type of the elements contained by the list (e.g., Int, String, etc).
*/
sealed abstract class Lista[+A] {
  def head: A
  def tail: Lista[A]

  /** Concatenation function.
  * @param other the other list with which to concatenate.
  */
  def ::: [B >: A](other: Lista[B]): Lista[B]

  /** Maps the elements of the list from their original type A, to a type B.
  * @param f the mapping function from A to B.
  */
  def map[B](f: Callable[A, B]): Lista[B]

  /** Similar to map but, in this case, the mapping function will change values
  * of a type A, to an Lista with elements of type B.
  * @param f the mapping function from A to Lista[B].
  */
  def flatMap[B](f: A => Lista[B]): Lista[B]
}

/** Structure for a non-empty list.
* @param A The type of elements contained in the list.
* @param head The head of the list, i.e., first element of the list.
* @param tail The tail (rest) of the list.
*/
case class Cons[A](head: A, tail: Lista[A]) extends Lista[A] {
  def ::: [B >: A](other: Lista[B]): Lista[B] = other match {
    case Empty => this
    case Cons(h, t) => Cons(h, :::(t))
  }

  def map[B](f: Callable[A, B]): Lista[B] = Cons(f(head), tail.map(f))

  def flatMap[B](f: A => Lista[B]): Lista[B] = f(head) ::: tail.flatMap(f)

  override def toString: String = {
    def loop(l: Lista[A] = this, acc: String = ""): String = l match {
      case Cons(h, Empty) => s"$acc$h"
      case Cons(h, t) => loop(t, s"$acc$h, ")
      case Empty => ""
    }

    s"Lista(${loop()})"
  }
}

/** Structure for an empty list.
* @param A Type of elements in the list. This is needed even if it is empty,
* because we need to provide it to the Lista super class. This can be
* avoided as shown in the Lista.
*/
case object Empty extends Lista[Nothing] {
  def head: Nothing = throw new Exception("Empty has no head")
  def tail: Lista[Nothing] = throw new Exception("Empty has no tail")
  def ::: [B >: Nothing](other: Lista[B]): Lista[B] = other
  def map[B](f: Callable[Nothing, B]): Lista[B] = Empty
  def flatMap[B](f: Nothing => Lista[B]): Lista[B] = Empty
}

object Lista {
  /** A Lista constructor function, e.g., Lista(1, 2, 3).
  *
  * @param ns a list of parameters of the same type (we need covariance if we'd
  * like to enable the use of different types within the same list).
  */
  def apply[A](ns: A*): Lista[A] =
    if (ns.isEmpty) Empty
    else Cons(ns.head, apply(ns.tail:_*))
}
