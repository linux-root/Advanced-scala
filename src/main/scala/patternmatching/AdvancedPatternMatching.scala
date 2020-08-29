package patternmatching

object AdvancedPatternMatching extends App {

 /**
  * 1. Pattern matching for class
  */
 class Person(val name: String,val age: Int)
 object Person{
  // using unapply method for deconstructing Person
  def unapply(p: Person): Option[(String, Int)] = Some(p.name, p.age)
 }

 val bob = new Person("Bob", 24)
 val greetingBob = bob match {
  case Person(name, age) => s"Hi, my name is $name, I am $age years old"
 }
 println(greetingBob)

 /***
  * 2. multiple unapply methods in Pattern matching
  */
 class Number(val value: Int)

 object OddNumber{
  def unapply(number: Number): Option[String] = if (number.value % 2 != 0) Some ("an odd number") else None
 }

 object EvenNumber{
  def unapply(number: Number): Option[String] = if (number.value % 2 == 0) Some ("an even number") else None
 }


 object SingleDigitNumber{
  def unapply(number: Number): Option[String] = if (number.value > - 10 && number.value < 10) Some ("a single digit number") else None
 }

 val number = new Number(2)

 val mathProperty =  number match {
  case OddNumber(p) => p
  case EvenNumber(p) => p
  case SingleDigitNumber(p) => p
  case _ => "No property"
 }
 println(mathProperty)
}
