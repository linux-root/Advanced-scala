package implycit

object Implycit extends App {

  case class Person(name : String, age: Int)

  val people = List(
    Person("Waton", 30),
    Person("Cooper", 22),
    Person("Kaycee", 33),
    Person("Kevin", 32),
    Person("Alita", 22)
  )

  implicit val sortingDef = Ordering.fromLessThan[Person]((p1, p2) => p1.name < p2.name)

  println(people.sorted)

  case class Purchase(nUnits : Int, unitPrice : Double){
    private def totalPrice = nUnits*unitPrice
  }
  object Purchase {
    implicit val totalPriceOdering : Ordering[Purchase] = Ordering.fromLessThan((p1, p2) => p1.unitPrice < p2.unitPrice)
  }

  object OrderingByUnitCount {
    implicit val orderingByUnitCount : Ordering[Purchase] = Ordering.fromLessThan[Purchase]((p1, p2) => p1.nUnits < p2.nUnits)
  }

  import OrderingByUnitCount._ // without importing this, implicit val will be searched inside object Purchase

  println(List[Purchase](
    Purchase(3, 4.3),
    Purchase(4, 2.4),
    Purchase(100, 1.2)
  ).sorted)




}

