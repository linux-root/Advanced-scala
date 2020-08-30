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


 /**
  * 3. Infix pattern
  */

 case class Or[A, B](a : A, b :B)
 val either = Or(5, "five")
 val humanDescription = either match {
  case number Or text => s"$number is written as $text" // alternative ways for : Or(number, text)
 }
 println(humanDescription)


 /**
  * 4. unapplySeq method
  */
  class SundayList[+A]{
   def head : A = ??? // implemented as nothing
   def tail : SundayList[A] = ??? // implemented as nothing
 }

 case object Empty extends SundayList[Nothing]
 case class Cons[+A](override val head : A, override  val tail : SundayList[A]) extends SundayList[A]

 object SundayList{
  def unapplySeq[A](list : SundayList[A]) : Option[Seq[A]] = {
     if (list == Empty) {
       Some(Seq.empty)
     } else {
       unapplySeq(list.tail).map(list.head +: _)
     }
  }
 }

 val sundayList = Cons(1, Cons(2, Cons(3, Cons(4, Empty))))

 sundayList match {
  case SundayList(1,2, _*) => println("start by 1, 2")
 }

 /**
  * 5. customized wrapper
  * normally, just use Option
  */
 class Student(val name: String,val age: Int)
 object Student{
  def unapply(p: Student): Wrapper[String] = new Wrapper[String] {
   override def isEmpty: Boolean = false
   override def get: String = p.name
  }
 }
 abstract class Wrapper[A]{
  def isEmpty : Boolean // required method
  def get : A // required method
 }

 val student = new Student("Alice", 23)

 student match {
  case Student(name) => println(s"Hi, I am $name")
 }



}
