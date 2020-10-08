/**
* Author: Elio Ventocilla
*/
package contravariance

import common._


object Contravariance extends App {
  val p = new Person("Julia")
  val s1 = new Student("Pedro", 1)
  val s2 = new Student("Maria", 2)
  val e1 = new Employee("Luisa", 20000.0)
  val e2 = new Employee("Jose", 15000.0)

  /** This is a "function" which takes a object Person, and returns its name.
  */
  val getName = new Callable[Person, String] {
    def apply(o: Person): String = o.name
  }

  /** The following would be the obviously valid call, where we pass a
  * person as a parameter to the getName "o" parameter.
  */
  getName(p)    // == "Julia"

  /** Since Callable is contravariant in the first type parameter (-A), then
  * the type pararemeter Person in getName can morph (change) to any of its
  * subclasses (i.e., Student and Employee). So the following calls are also valid:
  */
  getName(s1)   // == "Pedro"
  getName(e1)   // == "Luisa"


  // ----- An example with MyList ------

  val students: Lista[Student] = Lista(s1, s2)
  val employees: Lista[Employee] = Lista(e1, e2)
  val persons: Lista[Person] = students ::: employees

  students.map(getName)
  employees.map(getName)
  persons.map(getName)
}
