import java.time.LocalDate
import java.time.format.DateTimeParseException

object Zad1_5 extends App {

  object Sex extends Enumeration {
    val Male, Female = Value
  }

  case class Person(
                     birthday: LocalDate,
                     sex: Sex.Value
                   )

  class Pesel(pesel: String) {

    protected val digits: List[Int] = pesel.toList map (_.asDigit)

    def check(): Boolean = {
      if (pesel.length != 11) return false
      val weights = List(1, 3, 7, 9, 1, 3, 7, 9, 1, 3, 1)
      val control = (weights zip digits) map ((x) => x._1 * x._2) sum
      val valid = (control % 10) == 0

      valid
    }

    protected def birth(): LocalDate = {
      LocalDate.parse(
        "%04d-%02d-%02d" format(year(), month(), day())
      )
    }

    protected def year(): Int = {
      val year = 10 * digits.head + digits(1)
      val month = 10 * digits(2) + digits(3)

      month match {
        case x if x > 80 && x < 93 => year + 1800
        case x if x > 0 && x < 13 => year + 1900
        case x if x > 20 && x < 33 => year + 2000
        case x if x > 40 && x < 53 => year + 2100
        case x if x > 60 && x < 73 => year + 2200
        case x => x
      }
    }

    protected def month(): Int = {
      val month = 10 * digits(2) + digits(3)

      month match {
        case x if x > 80 && x < 93 => x - 80
        case x if x > 20 && x < 33 => x - 20
        case x if x > 40 && x < 53 => x - 40
        case x if x > 60 && x < 73 => x - 60
        case x => x
      }
    }

    protected def day(): Int = {
      10 * digits(4) + digits(5)
    }

    protected def sex(): Sex.Value = {
      if (digits(9) % 2 == 1) Sex.Male else Sex.Female
    }

    def toPerson: Option[Person] = {
      try {
        if (check())
          Some(Person(birth(), sex()))
        else
          None

      } catch {
        // may fail on date building, checksum is not everything
        case _: DateTimeParseException => None
      }
    }
  }

  val maybePersons = List(
    new Pesel("85112619133") toPerson, // Person(1985-11-26,Male)
    new Pesel("26040307866") toPerson, // Person(1926-04-03,Female)
    new Pesel("10020404867") toPerson, // Person(1910-02-04,Female)
    new Pesel("56101218035") toPerson, // Person(1956-10-12,Male)

    new Pesel("85112619131") toPerson, // bad checksum
    new Pesel("12561018035") toPerson, // proper checksum but bad date
    new Pesel("8511261913") toPerson, // too short
    new Pesel("561012180350") toPerson, // too long
    new Pesel("") toPerson // empty
  )

  maybePersons foreach println
}
