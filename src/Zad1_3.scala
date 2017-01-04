object Zad1_3 extends App {

  object Group extends Enumeration {
    val PiS, PO, N, PSL = Value
  }

  object Committee extends Enumeration {
    val Edukacji, Kultury, Sportu, UE = Value
  }

  case class Envoy(
                    name: String,
                    surname: String,
                    age: Int,
                    group: Group.Value,
                    function: String,
                    committees: Set[Committee.Value]
                  )

  val people = Vector(
    Envoy("Adam", "Nowak", 38, Group.PO, "marszałek", Set(Committee.Edukacji, Committee.Kultury)),
    Envoy("Zbyszek", "Kowalski", 55, Group.PSL, "wicemarszałek", Set(Committee.Sportu)),
    Envoy("Mariola", "Polna", 30, Group.N, "wicemarszałek", Set(Committee.Edukacji, Committee.UE)),
    Envoy("Anna", "Nowacka", 40, Group.PiS, "szef klubu", Set()),
    Envoy("Ignacy", "Krasicki", 27, Group.N, null, Set(Committee.Kultury))
  )

  val filtered = List(
    people filter (_.age > 40),
    people filter ('D' to 'K' contains _.surname(0)),
    people filter (_.committees contains Committee.Edukacji),
    people filter (_.committees.size >= 2),
    people filter (_.function == "marszałek"),
    people filter (x => (x.age > 40) && ('D' to 'K' contains x.surname(0)))
  )

  filtered foreach println
  assert(filtered(4).size == 1)
}
