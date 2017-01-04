

object Zad2_4 extends App {

  def time[R](block: => R): Long = {
    val t0 = System.nanoTime()
    block
    (System.nanoTime() - t0) / 1000
  }

  class MyList[+A] protected(
                              protected val list: List[A],
                              protected val _size: Int
                            ) extends Seq[A] {

    override def size: Int =
      _size

    override def length: Int =
      _size

    override def isEmpty: Boolean =
      _size == 0

    override def apply(idx: Int): A =
      list(idx)

    override def iterator: Iterator[A] =
      list.iterator

    def +:[B >: A, That](elem: B): MyList[B] =
      new MyList(list.+:(elem), _size + 1)

    def :+[B >: A](elem: B): MyList[B] =
      new MyList(list.:+(elem), _size + 1)

    def ::[B >: A](x: B): MyList[B] =
      new MyList(list.::(x), _size + 1)

    def :::[B >: A](prefix: MyList[B]): MyList[B] =
      new MyList(list.:::(prefix.list), _size + prefix._size)

    def :::[B >: A](prefix: List[B]): MyList[B] =
      new MyList(list.:::(prefix), _size + prefix.length)
  }

  object MyList {

    def apply[A](x: List[A]): MyList[A] =
      new MyList[A](x, x.length)

    def apply[A](x: A*): MyList[A] =
      new MyList[A](x.toList, x.length)
  }

  object MyNil extends MyList[Nothing](List(), 0) {

  }

  // Składnia zgodna z klasą List.

  val value: MyList[Int] = 0 :: (1 +: MyList(2, 3, 4) :+ 5) ::: MyList(6, 7) ::: 8 :: 9 :: MyNil
  value foreach (v => print(v.toString + " ")) // 0 1 2 3 4 5 6 7 8 9
  println

  // Szybkie określenie rozmiaru.
  // W przypadku gdy "MyList" jest budowane na podstawie "List"
  // koszt określenia rozmiaru został przerzucony na czas konstrukcji obiektu(!).

  val baseList: List[Int] = List.fill(5000000)(0)
  val myList1: MyList[Int] = MyList(baseList)
  val myList2: MyList[Int] = myList1 :+ 4

  println("(0) List.length: " + time(baseList.length) + "μs") // (0) List.length: 33747μs
  println("(1) MyList.length: " + time(myList1.length) + "μs") // (1) MyList.length: 90μs
  println("(2) MyList.length: " + time(myList2.length) + "μs") // (2) MyList.length: 50μs
}
