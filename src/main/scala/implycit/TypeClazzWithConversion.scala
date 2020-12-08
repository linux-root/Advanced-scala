package implycit

object TypeClazzWithConversion extends App {
   trait Equalizer[T]{
    def apply(a : T, b : T) : Boolean
  }

  implicit class Conversion[T](value : T){
     def ===(other: T)(implicit equalizer : Equalizer[T]): Boolean = equalizer(value, other)
  }

  implicit object PersonEqualizer extends Equalizer[Person]{
     override def apply(a: Person, b: Person): Boolean = a.name.equals(b.name)
   }

  def compareAndPrint[T : Equalizer](a : T, b : T) : Unit = { // sugar syntax : don't need to specify PersonEqualizer
    val eq = implicitly[Equalizer[T]]
     if (a === b){
       println("2 objects are equal")
     } else {
       println("2 objects are NOT equal")
     }
  }

  case class Person(name : String, age: Int)

  val watson = Person("Watson", 22)
  val Kaycee = Person("Kaycee", 32)

  println(watson === Kaycee)
  compareAndPrint(watson, Kaycee)
}
