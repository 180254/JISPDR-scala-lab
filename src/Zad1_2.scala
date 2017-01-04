import scala.collection.immutable.Seq

object Zad1_2 extends App {

  def isPerfect(num: Int): Boolean = {
    val root = math.sqrt(num) toInt
    val maybeDivisors = 1 to root

    val divisorsPart1: Seq[Int] = maybeDivisors filter (num % _ == 0) filter (_ != num)
    val divisorsPart2: Seq[Int] = divisorsPart1 map (num / _) filter (_ != num)

    val divisors = (divisorsPart1 ++ divisorsPart2) distinct
    val divisorsSum = divisors sum

    num == divisorsSum
  }

  (1 to 10000) filter isPerfect foreach println

  val somePerfects = List(6, 28, 496, 8128)
  assert(somePerfects forall isPerfect)
}
