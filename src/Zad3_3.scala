import scala.collection.mutable

object Zad3_3 extends App {

  implicit class ListA[T](val list: List[T]) {

  	// T = element kolekcji
    def processList[Res, Mid, Ret] // wynik [przekształcenia,cząstkowy,końcowy]
    (f1: (T, Res) => Mid) // rezultat cząstkowy na podstawie elementu kolekcji oraz jego wartości przekształconej
    (f2: List[Mid] => Ret) // formuje końcowy rezultat na podstawie rezultatów cząstkowych
    (f0: T => Res): Ret // funkcja przekształcająca (główna)
    = {
      val middles = new mutable.ListBuffer[Mid]

      for (elem <- list) {
        middles += f1(elem, f0(elem))
      }

      f2(middles.toList)
    }

    def a_map[B]: ((T) => B) => List[B] =
      processList[B, B, List[B]] {
        (_, result) => result
      } {
        middles => middles
      }

    def a_filter: (T => Boolean) => List[T] =
      processList[Boolean, (T, Boolean), List[T]] {
        (element, result) => (element, result)
      } {
        middles =>
          val result = new mutable.ListBuffer[T]

          for (mid <- middles if mid._2) {
            result += mid._1
          }

          result.toList
      }

    def a_forall: (T => Boolean) => Boolean =
      processList[Boolean, Boolean, Boolean] {
        (_, result) => result
      } {
        middles => !(middles contains false)
      }

    def a_exists: (T => Boolean) => Boolean =
      processList[Boolean, Boolean, Boolean] {
        (_, result) => result
      } {
        middles => middles contains true
      }
  }

  val someList = List(1, 2, 3, 2, 4, 5)

  someList a_map (_ * 3) foreach (i => print(i + ",")) // 3,6,9,6,12,15,
  println

  someList a_filter (_ >= 3) foreach (i => print(i + ",")) // 3,4,5,
  println

  println(someList a_forall (_ >= 3)) // false
  println(someList a_forall (_ >= 0)) // true
  println(someList a_exists (_ == 0)) // false
  println(someList a_exists (_ == 3)) // true
}
