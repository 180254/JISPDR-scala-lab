import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.xml.{Elem, Node, PrettyPrinter, XML}

// http://alvinalexander.com/scala/how-to-extract-data-from-xml-nodes-in-scala

object Zad1_6 extends App {

  val path: String = "C:\\Users\\Adrian\\Desktop\\scala\\src\\"
  val xml: Elem = XML.loadFile(path + "Zad1_6_studenci.xml")

  type Student = Node
  type Predicate[T] = T => Boolean
  type FileName = String

  val students =
    xml \ "student"

  val birthYear =
    (student: Student) => (student attribute "ur" getOrElse "0").toString toInt

  val friends =
    (student: Student) => student \ "znajomy"

  val friendsCount =
    (student: Student) => friends(student).length

  val filters =
    List[(Predicate[Student], FileName)](
      (friendsCount(_) == 0, "Zad1_6_X0_zero.xml"),
      (friendsCount(_) == 1, "Zad1_6_X1_one.xml"),
      (friendsCount(_) == 2, "Zad1_6_X2_two.xml"),
      (friendsCount(_) == 3, "Zad1_6_X3_three.xml"),
      (friendsCount(_) >= 4, "Zad1_6_X4_many.xml")
    )

  val xmlPrettyPrinter = new PrettyPrinter(80, 2)

  filters foreach {
    x =>
      val predicate = x._1
      val fileName = x._2

      val students_f = students filter (predicate(_)) sortBy (birthYear(_))

      val root =
        <studenci>
          {students_f}
        </studenci>

      // output is not formatted
      // XML.save(path + fileName, root, "UTF-8")

      // formatted output
      val formatted = xmlPrettyPrinter.format(root)
      val fPath = Paths.get(path + fileName)
      val fWriter = Files.newBufferedWriter(fPath, StandardCharsets.UTF_8)
      fWriter.write(formatted)
      fWriter.close();
  }
}

