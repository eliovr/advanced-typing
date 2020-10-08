/**
* Author: Elio Ventocilla
*/
package implicits

object Chef {
  def makeDish(main: String)(implicit context: Context): String = {
    context match {
      case Swedish => s"$main with smashed potatoes"
      case Indian => s"$main with rice"
    }
  }
}
