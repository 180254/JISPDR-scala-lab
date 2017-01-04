
object Zad3_1 extends App {

  implicit class ListA[T](val list: List[T]) {

    def a_map[B](f: T => B): List[B] = {
      list match {
        case head :: tail => f(head) :: (tail a_map f)
        case _ => Nil
      }
    }

    def a_filter(f: T => Boolean): List[T] = {
      list match {
        case head :: tail if f(head) => head :: (tail a_filter f)
        case _ :: tail => tail a_filter f
        case _ => Nil
      }
    }
  }

  val someList = List(1, 2, 3, 2, 4, 5)

  someList a_map (_ * 3) foreach (i => print(i + ",")) // 3,6,9,6,12,15,
  println

  someList a_filter (_ >= 3) foreach (i => print(i + ",")) // 3,4,5,
  println
}
