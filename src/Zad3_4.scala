import scala.runtime.Nothing$

object Zad3_4 extends App {

  abstract class ColCreator[T, Col >: Seq[T]] {
    def create(): Col
  }

  class ListCreator[T] extends ColCreator[T, List[T]] {
    override def create(): List[T] = {
      List[T]()
    }
  }

  //implicit val listim[T]: ColCreator[T, List[T]] = new ListCreator[T]

  implicit val listim1: ColCreator[Int, List[Int]] = new ListCreator[Int]

  implicit class SeqA[T, That >: Seq[T]](val list: That) {
    def a_map[B](f: T => B)(implicit bf: ColCreator[B, That]): That = {
      println(bf.create())
      bf.create()
    }
  }



  val someList: List[Int] = List(1, 2, 3, 2, 4, 5)
  private val a1 = new SeqA[Int, List[Int]](someList)
  a1 a_map[Int] (a => a)(new ListCreator[Int]())
  println
}
