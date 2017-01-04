import scala.io.BufferedSource
import scala.util.Try

object Zad2_2 extends App {

  type Predicate[T] = T => Boolean

  val path: String = """C:\Users\Adrian\Desktop\scala\src\Zad2_2_wydatki.csv"""
  val source: BufferedSource = io.Source.fromFile(path)

  val isCommentLine: Predicate[String] = line => line startsWith ";"
  val isEmptyLine: Predicate[String] = line => line.trim isEmpty

  val dataLines: Iterator[String] = source.getLines drop 1 filterNot isCommentLine filterNot isEmptyLine
  val dataTokens: Vector[Array[String]] = dataLines map (_ split "," map (_.trim)) toVector

  if (dataTokens exists (_.length < 4)) {
    throw new Exception("incorrect input file; required at least 4 columns in each row")
  }

  if (dataTokens flatMap (_.slice(1, 4)) map (n => Try(n.toDouble)) exists (_.isFailure)) {
    throw new Exception("incorrect input file; columns 2-4 must be parsable doubles")
  }

  val widths: Seq[Int] = (0 to 3) map (i => dataTokens map (_ (i) replaceAll("\\.[0-9]", "") length) max)

  dataTokens foreach { lineTokens =>
    val Array(month, revenue, expenses, profit, _*) = lineTokens
    val formatted = String.format("%s: %s - %s = %s",
      "%-" + widths.head + "s" format month,
      "%" + widths(1) + ".0f" format revenue.toDouble,
      "%" + widths(2) + ".0f" format expenses.toDouble,
      "%" + widths(3) + ".0f" format profit.toDouble
    )

    println(formatted)

    /*
      styczeń :  10000 -   9300 =   700
      luty    :  10801 -   9500 =  1301
      marzec  :  12000 -  10500 =  1500
      kwiecień:   1000 -   8800 =  2200
     */
  }
}
