/**
* Author: Elio Ventocilla
*/
package contravariance

/** Callable is data data structure resembling a Scala function with one parameter.
* The first parameter type (-A) defines the type of that function's parameter
* (i.e., "x"'s type), and the second parameter type (B) defines the type of the
* function's return value.
*
* -A is contravariant, which means that "x" can be of some type A, or of any subclass
* of A. Removing the minus (-) would make A invariant, i.e., "x" would only be
* able to be of type A, and NOT any of its subclases.
*/
trait Callable[-A, B] {
  def apply(x: A): B
}
