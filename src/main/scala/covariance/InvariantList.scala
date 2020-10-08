/**
* Author: Elio Ventocilla
*/
package covariance

/**
* An implementation of an invariant list. This data structure will allow us
* to create lists with elements of a given type (e.g., Ints, Strings, Booleans,
* etc), but will not let us have elements of different types within the same list.
* For example, we cannot do InvariantList(1, "one", true). To enable such behavior,
* we require covariance (see CovariantList.scala).
*
* @param A The type of the elements contained by the list (e.g., Int, String, etc).
*/
sealed abstract class InvariantList[A] {
  def head: A
  def tail: InvariantList[A]

  /** Concatenation function.
  * @param other the other list with which to concatenate.
  */
  def ::: (other: InvariantList[A]): InvariantList[A]

  /** Maps the elements of the list from their original type A, to a type B.
  * @param f the mapping function from A to B.
  */
  def map[B](f: A => B): InvariantList[B]

  /** Similar to map but, in this case, the mapping function will change values
  * of a type A, to an InvariantList with elements of type B.
  * @param f the mapping function from A to InvariantList[B].
  */
  def flatMap[B](f: A => InvariantList[B]): InvariantList[B]
}

/** Structure for a non-empty list.
* @param A The type of elements contained in the list.
* @param head The head of the list, i.e., first element of the list.
* @param tail The tail (rest) of the list.
*/
case class InvariantCons[A](head: A, tail: InvariantList[A]) extends InvariantList[A] {
  def ::: (other: InvariantList[A]): InvariantList[A] = other match {
    case InvariantEmpty() => this
    case InvariantCons(h, t) => InvariantCons(h, :::(t))
  }

  def map[B](f: A => B): InvariantList[B] = InvariantCons(f(head), tail.map(f))

  def flatMap[B](f: A => InvariantList[B]): InvariantList[B] = f(head) ::: tail.flatMap(f)

  override def toString: String = {
    def loop(l: InvariantList[A] = this, acc: String = ""): String = l match {
      case InvariantCons(h, InvariantEmpty()) => s"$acc$h"
      case InvariantCons(h, t) => loop(t, s"$acc$h, ")
      case InvariantEmpty() => ""
    }

    "InvariantList(" + loop() + ")"
  }
}

/** Structure for an empty list.
* @param A Type of elements in the list. This is needed even if it is empty,
* because we need to provide it to the InvariantList super class. This can be
* avoided as shown in the CovariantList.
*/
case class InvariantEmpty[A]() extends InvariantList[A] {
  def head: A = throw new Exception("InvariantEmpty has no head")
  def tail: InvariantList[A] = throw new Exception("InvariantEmpty has no tail")
  def ::: (e: InvariantList[A]): InvariantList[A] = e
  def map[B](f: A => B): InvariantList[B] = InvariantEmpty[B]()
  def flatMap[B](f: A => InvariantList[B]): InvariantList[B] = InvariantEmpty[B]()
}

object InvariantList {
  /** A InvariantList constructur function, e.g., InvariantList(1, 2, 3).
  *
  * @param ns a list of parameters of the same type (we need covariance if we'd
  * like to enable the use of different types within the same list).
  */
  def apply[A](ns: A*): InvariantList[A] =
    if (ns.isEmpty) InvariantEmpty[A]()
    else InvariantCons(ns.head, apply(ns.tail:_*))
}
