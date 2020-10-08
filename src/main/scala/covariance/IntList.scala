/**
* Author: Elio Ventocilla
*/
package covariance

/**
* An implementation of list of integers. This data structure will allow us
* to create lists of only integers.
*
* To create lists with other types of elements see InvariantList.
* To create lists with different types of elements within the same list, see
* CovariantList.
*
*/
sealed abstract class IntList {
  def head: Int
  def tail: IntList

  /** Concatenation function.
  * @param other the other list with which to concatenate.
  */
  def ::: (other: IntList): IntList

  /** Maps/transforms the elements of the list.
  * @param f the mapping/transformation function.
  */
  def map(f: Int => Int): IntList

  /** Similar to map but, in this case, the mapping function will change values
  * from Int to an IntList (not from Int to Int as in map).
  * @param f the mapping/transformation function from Int to IntList.
  */
  def flatMap(f: Int => IntList): IntList
}

/** Structure for a non-empty list.
*
* @param head The head of the list, i.e., first element of the list.
* @param tail The tail (rest) of the list.
*/
case class IntCons(head: Int, tail: IntList) extends IntList {
  def ::: (other: IntList): IntList = other match {
    case IntEmpty => this
    case IntCons(h, t) => IntCons(h, :::(t))
  }

  def map(f: Int => Int): IntList = IntCons(f(head), tail.map(f))

  def flatMap(f: Int => IntList): IntList = f(head) ::: tail.flatMap(f)

  override def toString: String = {
    def loop(l: IntList = this, acc: String = ""): String = l match {
      case IntCons(h, IntEmpty) => s"$acc$h"
      case IntCons(h, t) => loop(t, s"$acc$h, ")
      case IntEmpty => ""
    }

    "IntList(" + loop() + ")"
  }
}

/** Structure for an empty list.
*
* <Some empty space to keep the code in the same position as the other list
* examples, so you can Ctrl+tab and compare>.
*/
object IntEmpty extends IntList {
  def head: Int = throw new Exception("IntEmpty has no head")
  def tail: IntList = throw new Exception("IntEmpty has no tail")
  def ::: (e: IntList): IntList = e
  def map(f: Int => Int): IntList = IntEmpty
  def flatMap(f: Int => IntList): IntList = IntEmpty
}

object IntList {
  /** A IntList constructur function, e.g., IntList(1, 2, 3).
  *
  * @param ns a list of parameters of the same type (we need covariance if we'd
  * like to enable the use of different types within the same list).
  */
  def apply(ns: Int*): IntList =
    if (ns.isEmpty) IntEmpty
    else IntCons(ns.head, apply(ns.tail:_*))
}
