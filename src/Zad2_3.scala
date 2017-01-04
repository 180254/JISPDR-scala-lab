
object Zad2_3 extends App {

  def time[R](block: => R): Long = {
    val t0 = System.nanoTime()
    block
    (System.nanoTime() - t0) / 1000
  }

  val size: Int = 1000000

  val list: List[Int] = List.fill(size)(0)
  val vector: Vector[Int] = Vector.fill(size)(0)

  println("List.tail: " + time(list.tail) + "μs")
  println("List.length: " + time(list.length) + "μs")
  println("List.append: " + time(list :+ 1) + "μs")
  println("List.prepend: " + time(1 +: list) + "μs")

  println("Vector.tail: " + time(vector.tail) + "μs")
  println("Vector.length: " + time(vector.length) + "μs")
  println("Vector.append: " + time(vector :+ 1) + "μs")
  println("Vector.prepend: " + time(1 +: vector) + "μs")

  /*
    List.tail: 37μs
    List.length: 19212μs
    List.append: 204256μs
    List.apply: 39μs
    Vector.tail: 208μs
    Vector.length: 126μs
    Vector.append: 162μs
    Vector.apply: 1507μs
  */

  // List: length i append są wyraźnie kosztowane - wymagają iteracji przez listę.
  // List: tail i prepend wymagają jedynie znajomości pierwszego elementu i są wykonywanie w czasie stałym.
  // Vector jest pozbawiony tych wad - wszystkie operacje są wykonywane w czasie efektywnie stałym.
}
