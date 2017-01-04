import scala.util.Try

object Zad2_5 extends App {

  def time[R](block: => R): (Long, Long, Long) = {
    val t0 = System.nanoTime()
    block
    val t1 = (System.nanoTime() - t0) / 1000

    val t2 = System.nanoTime()
    block
    val t3 = (System.nanoTime() - t2) / 1000

    val t4 = System.nanoTime()
    block
    val t5 = (System.nanoTime() - t4) / 1000

    (t1, t3, t5)
  }

  // -----------------------------------------------------------------------------------------------------------------

  implicit class ListAdditions[A](list: List[A]) {

    // Implementacja z wykorzystaniem właściwości size.
    def std_isLongerThanN(n: Int): Boolean = list.size > n

    def std_>=(n: Int): Boolean = list.size >= n

    def std_isShorterThanN(n: Int): Boolean = list.size < n

    def std_<=(n: Int): Boolean = list.size <= n


    // ---------------------------------------------------------------------

    // Implementacja z wykorzystaniem iteracji metodą apply() do odpowiedniego elementu i łapaniem wyjątków.
    def exc_isLongerThanN(n: Int): Boolean = Try(list(n)).isSuccess

    def exc_>=(n: Int): Boolean = Try(list(n - 1)).isSuccess

    def exc_isShorterThanN(n: Int): Boolean = Try(list(n - 1)).isFailure

    def exc_<=(n: Int): Boolean = Try(list(n)).isFailure


    // ---------------------------------------------------------------------

    // Implementacja z wykorzystaniem iteracji pętlą for do odpowiedniego elementu i przerywaniem pętli.
    def for_isLongerThanN(n: Int): Boolean = {
      var i = 0
      for (_ <- list) {
        i += 1; if (i > n) return true
      }
      false
    }

    def loo_>=(n: Int): Boolean = {
      var i = 0
      for (_ <- list) {
        i += 1; if (i >= n) return true
      }
      false
    }

    def for_isShorterThanN(n: Int): Boolean = {
      var i = 0
      for (_ <- list) {
        i += 1; if (i >= n) return false
      }
      true
    }

    def for_<=(n: Int): Boolean = {
      var i = 0
      for (_ <- list) {
        i += 1; if (i > n) return false
      }
      true
    }

  }

  // -----------------------------------------------------------------------------------------------------------------

  // Podstawowe asercje sprawdzające przygotowane implementacje.

  var someList = List(1, 2, 3, 4)

  assert(someList std_isLongerThanN 2)
  assert(someList std_isLongerThanN 3)
  assert(!(someList std_isLongerThanN 4))
  assert(someList std_isShorterThanN 6)
  assert(someList std_isShorterThanN 5)
  assert(!(someList std_isShorterThanN 4))
  assert(someList std_>= 3)
  assert(someList std_>= 4)
  assert(!(someList std_>= 5))
  assert(someList std_<= 5)
  assert(someList std_<= 4)
  assert(!(someList std_<= 3))

  assert(someList exc_isLongerThanN 2)
  assert(someList exc_isLongerThanN 3)
  assert(!(someList exc_isLongerThanN 4))
  assert(someList exc_isShorterThanN 6)
  assert(someList exc_isShorterThanN 5)
  assert(!(someList exc_isShorterThanN 4))
  assert(someList exc_>= 3)
  assert(someList exc_>= 4)
  assert(!(someList exc_>= 5))
  assert(someList exc_<= 5)
  assert(someList exc_<= 4)
  assert(!(someList exc_<= 3))

  assert(someList for_isLongerThanN 2)
  assert(someList for_isLongerThanN 3)
  assert(!(someList for_isLongerThanN 4))
  assert(someList for_isShorterThanN 6)
  assert(someList for_isShorterThanN 5)
  assert(!(someList for_isShorterThanN 4))
  assert(someList loo_>= 3)
  assert(someList loo_>= 4)
  assert(!(someList loo_>= 5))
  assert(someList for_<= 5)
  assert(someList for_<= 4)
  assert(!(someList for_<= 3))

  // -----------------------------------------------------------------------------------------------------------------

  // Pomiar czasu dla wszystkich kombinacji.
  // Każdy pomiar jest dokonywany trzykrotnie.

  val size: Int = 1000000
  val list: List[Int] = List.fill(size)(0)
  val begin: Int = (size * 0.001) toInt
  val middle: Int = (size * 0.5) toInt
  val end: Int = (size * 0.9) toInt

  val tuples = Array(("beg", begin), ("mid", middle), ("end", end))

  println("size=" + size)
  println("begin=" + begin)
  println("middle=" + middle)
  println("end=" + end)

  println
  for (tup <- tuples) {
    println("[std] " + tup._1 + ".isLongerThanN: " + time(list.std_isLongerThanN(tup._2)) + "μs")
    println("[exc] " + tup._1 + ".isLongerThanN: " + time(list.exc_isLongerThanN(tup._2)) + "μs")
    println("[for] " + tup._1 + ".isLongerThanN: " + time(list.for_isLongerThanN(tup._2)) + "μs")
    println
  }

  for (tup <- tuples) {
    println("[std] " + tup._1 + ".isShorterThanN: " + time(list.std_isShorterThanN(tup._2)) + "μs")
    println("[exc] " + tup._1 + ".isShorterThanN: " + time(list.exc_isShorterThanN(tup._2)) + "μs")
    println("[for] " + tup._1 + ".isShorterThanN: " + time(list.for_isShorterThanN(tup._2)) + "μs")
    println
  }

  for (tup <- tuples) {
    println("[std] " + tup._1 + ".<=: " + time(list.std_<=(tup._2)) + "μs")
    println("[exc] " + tup._1 + ".<=: " + time(list.exc_<=(tup._2)) + "μs")
    println("[for] " + tup._1 + ".<=: " + time(list.for_<=(tup._2)) + "μs")
    println
  }

  for (tup <- tuples) {
    println("[std] " + tup._1 + ".>=: " + time(list.std_>=(tup._2)) + "μs")
    println("[exc] " + tup._1 + ".>=: " + time(list.exc_>=(tup._2)) + "μs")
    println("[for] " + tup._1 + ".>=: " + time(list.loo_>=(tup._2)) + "μs")
    println
  }

  // Zarówno implementacja z iterowaniem metodą apply() jak i z iterowaniem pętlą for są znacznie szybsze niż
  // implementacja naiwna z użyciem właściwości size. Implementacja naiwna (std) potrzebuje znać cały rozmiar,
  // aby określić prawdziwość warunku co jest bardzo niewydajne dla małych wartości n i dużych list.

  // Iteracje tylko do momentu, gdy jest to konieczne można zrealizować np. na dwa przedstawione sposoby.
  // Obydwa są znacząco szybsze z uwagi na wyeliminowanie podstawowej wady najprostszej implementacji.

  // Zaskoczeniem dla mnie jest, że apply() z łapaniem wyjątków jest o wiele szybsze niż iterowanie pętlą for.
  // Dla obydwu metod osiągane czasy są bardzo dobre dla małych i średnich n.
  // Dla dużych n czasy nie odbiegają znacznie od użycia właściwości size.

  /*
    size=1000000
    begin=1000
    middle=500000
    end=900000

    [std] beg.isLongerThanN: (18741,6757,6739)μs
    [exc] beg.isLongerThanN: (127,107,103)μs
    [for] beg.isLongerThanN: (1606,307,548)μs

    [std] mid.isLongerThanN: (6464,7863,6790)μs
    [exc] mid.isLongerThanN: (15834,7598,3278)μs
    [for] mid.isLongerThanN: (24666,3694,3490)μs

    [std] end.isLongerThanN: (6007,5762,8256)μs
    [exc] end.isLongerThanN: (6970,6298,5911)μs
    [for] end.isLongerThanN: (6018,7159,6281)μs

    [std] beg.isShorterThanN: (6922,6624,7685)μs
    [exc] beg.isShorterThanN: (65,5,4)μs
    [for] beg.isShorterThanN: (600,2481,395)μs

    [std] mid.isShorterThanN: (7697,9918,6607)μs
    [exc] mid.isShorterThanN: (3520,4076,3222)μs
    [for] mid.isShorterThanN: (3186,2852,3236)μs

    [std] end.isShorterThanN: (7711,7958,7046)μs
    [exc] end.isShorterThanN: (5475,5216,6162)μs
    [for] end.isShorterThanN: (5260,6058,6380)μs

    [std] beg.<=: (6120,6828,6918)μs
    [exc] beg.<=: (44,5,5)μs
    [for] beg.<=: (611,819,175)μs

    [std] mid.<=: (8297,6168,6631)μs
    [exc] mid.<=: (3109,3476,2801)μs
    [for] mid.<=: (13007,5124,4902)μs

    [std] end.<=: (10762,8684,7875)μs
    [exc] end.<=: (7144,6837,8602)μs
    [for] end.<=: (6592,8010,16991)μs

    [std] beg.>=: (22759,11469,12430)μs
    [exc] beg.>=: (44,6,6)μs
    [for] beg.>=: (1725,45,29)μs

    [std] mid.>=: (22221,20990,6279)μs
    [exc] mid.>=: (5160,3036,3068)μs
    [for] mid.>=: (9780,22304,13916)μs

    [std] end.>=: (16057,8406,9151)μs
    [exc] end.>=: (9685,11672,11925)μs
    [for] end.>=: (27181,14717,15029)μs
   */
}
