package implycit

object TypeClazz extends App{

 case class Person(name: String, age: Int)

  // trait
 trait Equal[T]{
  def equals(a : T, b :T ): Boolean
 }


 object Equal{
    def equals[T](a : T, b : T)(implicit equalizer : Equal[T]) : Boolean = equalizer.equals(a, b)
    def apply[T](implicit equalizer : Equal[T]) : Equal[T] = equalizer
 }

  // one of instance of type class Equal[T]
 implicit object PersonEquality extends Equal[Person]{
  override def equals(a: Person, b: Person): Boolean = a.name.equals(b.name) && a.age == b.age
 }
 // one of instance of type class Equal[T]
 implicit object IntEquality extends Equal[Int]{
  override def equals(a: Int, b: Int): Boolean = a == b
 }

 val person1 = Person("Watson", 23)
 val person2 = Person("Kaycee", 22)

 println(Equal.equals(person1, person2))
 println(Equal[Int].equals(2,2))

 implicit class RichString(val value : String) extends AnyVal{
   def encrypt(key: Int = 3): String = value
     .map(e => (e + key).toChar)
     .mkString
  def asInt : Int = value.toInt
 }

 implicit class RichInt(val value : Int) extends AnyVal{
   def times(f : () => Unit) : Unit = {
     for(_ <- 1 to this.value){
       f()
     }
   }
 }


 val encrypted = "Zdwvrq".encrypt(-3)
 println(encrypted)

 println("2323".asInt)

  4 times(() => println("Love"))
}
