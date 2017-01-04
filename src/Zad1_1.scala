object Zad1_1 extends App {

  def calc(expression: String): Int = {
    calc0(expression.split(" ").toList)
  }

  def calc0(tokens: List[String]): Int = {
    tokens match {
      case left :: "+" :: right :: rest => calc0((left.toInt + right.toInt).toString :: rest)
      case left :: "-" :: right :: rest => calc0((left.toInt - right.toInt).toString :: rest)
      case value :: Nil => value.toInt
      case _ => throw new IllegalArgumentException("arg was wrong ...")
    }
  }

  val result: Int = calc("-3 + 4 - 1 + 1 + 13 - 5 + 6")

  println(result)
  assert(result == 15)
}
