/**
* Author: Elio Ventocilla
*/
package object implicits {
  implicit class WrappedList[A](l: List[A]) {
    def toLista: Lista[A] = {
      lazy val loop: List[A] => Lista[A] = {
        case Nil => Empty
        case h :: t => Cons(h, loop(t))
      }
      loop(l)
    }
  }

  implicit class WrappedString(s: String) {
    def toLista: Lista[Char] = s.toList.toLista
  }
}
