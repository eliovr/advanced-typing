/**
* Author: Elio Ventocilla
*/
package implicits

import common._

object ImplicitsParameters extends App {

  // ---- Example 1

  /**
  * Check the obkect Chef (in Chef.scala). Note that the function "makeDish"
  * takes two parameters, a main course and a context. The former one is given
  * as usual, but the latter one has an implicit keyword.
  */
  implicit val context: Context = Swedish
  var dish: String = ""

  println("Asking the chef to cook 'meatballs' with an EXPLICIT context...")
  dish = Chef.makeDish("meatballs")(Indian)
  println(s"\t..making $dish")

  println("Asking the chef to cook 'meatballs' in the IMPLICIT context...")
  dish = Chef.makeDish("meatballs") // note that the context parameter is not given.
  println(s"\t..making $dish")


  // ---- Example 2

  /**
  * Check the ADT Lista (in the implicits package). Note that the function
  * isSorted also has an implicit parameter. This parameter tells the function
  * how to compare the objects in the list.
  * Scala imports makes this implict parameter available for lists of AnyVal
  * (Int, String, Char). For other types of objects, we have to create our own.
  */
  val ns1: Lista[Int] = Lista(1, 2, 3, 4, 5)  // sorted
  val ns2: Lista[Int] = Lista(1, 2, 5, 4, 3)  // not sorted

  // Note that we don't have to provide the "order" parameter.
  var sorted: Boolean = ns1.isSorted
  println(s"Is $ns1 sorted? $sorted")

  sorted = ns2.isSorted
  println(s"Is $ns2 sorted? $sorted")

  /**
  * Scala does not provide a way to compare objects of type Person,
  * so we have to provide it ourselves.
  */
  val s1: Lista[Student] = Lista(new Student("A", 1), new Student("B", 2), new Student("C", 3)) // sorted
  val s2: Lista[Student] = Lista(new Student("B", 2), new Student("A", 1), new Student("C", 3)) // not sorted

  implicit val personOrdering = new Ordering[Student]{
    def compare(a: Student, b: Student): Int = a.id - b.id
  }

  sorted = s1.isSorted
  println(s"Is $s1 sorted? $sorted")

  sorted = s2.isSorted
  println(s"Is $s2 sorted? $sorted")

}
