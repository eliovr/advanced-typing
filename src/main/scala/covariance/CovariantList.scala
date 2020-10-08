/**
* Author: Elio Ventocilla
*/
package covariance

/**
* An implementation of an COvariant list. This data structure will allow us
* to create lists with elements of a "different" types (Ints, Strings, Booleans,
* etc), e.g., CovariantList(1, "one", true).
*
* @param A The type of the elements contained by the list (e.g., Int, String, etc).
*   This parameter type is covariant, which means that a list of type
*   CovariantList[A] can morph/change into a list of type CovariantList[X],
*   as long as X == A or X is a superclass of A.
*/
sealed abstract class CovariantList[+A] {
  def head: A
  def tail: CovariantList[A]

  /** Concatenation function.
  * @param other the other list with which to concatenate.
  */
  def ::: [B >: A](other: CovariantList[B]): CovariantList[B]

  /** Maps the elements of the list from their original type A, to a type B.
  * @param f the mapping function from A to B.
  */
  def map[B](f: A => B): CovariantList[B]

  /** Similar to map but, in this case, the mapping function will change values
  * of a type A, to an CovariantList with elements of type B.
  * @param f the mapping function from A to CovariantList[B].
  */
  def flatMap[B](f: A => CovariantList[B]): CovariantList[B]
}

/** Structure for a non-empty list.
* @param A The type of elements contained in the list.
* @param head The head of the list, i.e., first element of the list.
* @param tail The tail (rest) of the list.
*/
case class CovariantCons[A](head: A, tail: CovariantList[A]) extends CovariantList[A] {
  def ::: [B >: A](other: CovariantList[B]): CovariantList[B] = other match {
    case CovariantEmpty => this
    case CovariantCons(h, t) => CovariantCons(h, :::(t))
  }

  def map[B](f: A => B): CovariantList[B] = CovariantCons(f(head), tail.map(f))

  def flatMap[B](f: A => CovariantList[B]): CovariantList[B] = f(head) ::: tail.flatMap(f)

  override def toString: String = {
    def loop(l: CovariantList[A] = this, acc: String = ""): String = l match {
      case CovariantCons(h, CovariantEmpty) => s"$acc$h"
      case CovariantCons(h, t) => loop(t, s"$acc$h, ")
      case CovariantEmpty => ""
    }

    "CovariantList(" + loop() + ")"
  }
}

/** Structure for an empty list.
* @param A Type of elements in the list. This is needed even if it is empty,
* because we need to provide it to the CovariantList super class. This can be
* avoided as shown in the CovariantList.
*/
case object CovariantEmpty extends CovariantList[Nothing] {
  def head: Nothing = throw new Exception("CovariantEmpty has no head")
  def tail: CovariantList[Nothing] = throw new Exception("CovariantEmpty has no tail")
  def ::: [B >: Nothing](other: CovariantList[B]): CovariantList[B] = other
  def map[B](f: Nothing => B): CovariantList[B] = CovariantEmpty
  def flatMap[B](f: Nothing => CovariantList[B]): CovariantList[B] = CovariantEmpty
}

object CovariantList {
  /** A CovariantList constructor function, e.g., CovariantList(1, 2, 3).
  *
  * @param ns a list of parameters of the same type (we need covariance if we'd
  * like to enable the use of different types within the same list).
  */
  def apply[A](ns: A*): CovariantList[A] =
    if (ns.isEmpty) CovariantEmpty
    else CovariantCons(ns.head, apply(ns.tail:_*))
}
