object Zad1_4 extends App {

  def func(values: List[String]): String = {
    "{" + func0(values) + "}"
  }

  def func0(values: List[String]): String = {
    values match {
      case left :: right :: Nil => left + " i " + right
      case left :: right :: rest => func0(left + ", " + right :: rest)
      case a :: Nil => a
      case _ => ""
    }
  }

  println(func(List("A", "B", "C", "D")))

  assert(func(List()) == "{}")
  assert(func(List("A")) == "{A}")
  assert(func(List("A", "B")) == "{A i B}")
  assert(func(List("A", "B", "C")) == "{A, B i C}")
  assert(func(List("A", "B", "C", "D")) == "{A, B, C i D}")
}
