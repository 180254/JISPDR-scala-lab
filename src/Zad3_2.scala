
object Zad3_2 extends App {

  implicit class ListA[T](val list: List[T]) {

    def a_forall(f: T => Boolean): Boolean = {
      list match {
        case head :: tail => f(head) && (tail a_forall f)
        case _ => true
      }
    }

    def a_exists(f: T => Boolean): Boolean = {
      list match {
        case head :: tail => f(head) || (tail a_exists f)
        case _ => false
      }
    }
  }

  val someList = List(1, 2, 3, 2, 4, 5)

  println(someList a_forall (_ >= 3)) // false
  println(someList a_forall (_ >= 0)) // true
  println(someList a_exists (_ == 0)) // false
  println(someList a_exists (_ == 3)) // true
}
