/**
* Author: Elio Ventocilla
*/
package implicits

object ImplicitConversions extends App {
  /**
  * Check the Wrappers.scala file. It contains two wrappers, one for objects of
  * type Lista[_], and another for objects of type String.
  * With these two we can now convert objects of those types, into objects of
  * out own class Lista.
  */

  /**
  * Note that List does not explicitly implement the function toLista.
  */
  val ns1 = List(1, 2, 3)
  println(s"Converting $ns1 into Lista...")
  val ns2 = ns1.toLista
  println(s"\t..$ns2")

  /**
  * Similarly with the following example.
  */
  val str = "Some string text"
  println(s"Converting [$str] into Lista...")
  val strLista = str.toLista
  println(s"\t..$strLista")
}
