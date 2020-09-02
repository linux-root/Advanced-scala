package functionalprogramming

import scala.annotation.tailrec

trait SundaySet[A] extends (A => Boolean){
 def &(other : SundaySet[A]) : SundaySet[A]
 def --(other: SundaySet[A]) : SundaySet[A]
 def -(element : A) : SundaySet[A]
 def contains(element: A) : Boolean
 def +(element:A) : SundaySet[A]
 def ++(other: SundaySet[A]) : SundaySet[A]
 def map[B](f : A => B) : SundaySet[B]
 def filter(predicate : A => Boolean) : SundaySet[A]
 def flatMap[B](f : A => SundaySet[B]) : SundaySet[B]
 def foreach(f : A => Unit)
 def unary_! : SundaySet[A]
}

class Empty[A] extends SundaySet[A] {
 override def contains(element: A): Boolean = false

 override def &(other: SundaySet[A]): SundaySet[A] = this

 override def --(other: SundaySet[A]): SundaySet[A] = other

 override def -(element: A): SundaySet[A] = this

 override def +(element: A): SundaySet[A] = new NonEmpty[A](element, this)

 override def ++(other: SundaySet[A]): SundaySet[A] = other

 override def map[B](f: A => B): SundaySet[B] = new Empty[B]

 override def filter(predicate: A => Boolean): SundaySet[A] = new Empty[A]

 override def apply(v1: A): Boolean = this.contains(v1)

 override def flatMap[B](f: A => SundaySet[B]): SundaySet[B] = new Empty[B]

 override def foreach(f: A => Unit): Unit = ()

 override def unary_! : SundaySet[A] = new PropertyBasedSet[A](_ => true)
}

class PropertyBasedSet[A](property : A => Boolean) extends SundaySet[A] {
 override def &(other: SundaySet[A]): SundaySet[A] = this.filter(other)

 override def --(other: SundaySet[A]): SundaySet[A] = this.filter(!other(_))

 override def -(element: A): SundaySet[A] = this.filter(_ != element)

 override def contains(element: A): Boolean = property(element)

 override def +(element: A): SundaySet[A] = new PropertyBasedSet[A]((x : A) => x == element || property(x))

 override def ++(other: SundaySet[A]): SundaySet[A] = new PropertyBasedSet[A](x => other(x) || property(x))

 override def map[B](f: A => B): SundaySet[B] = politelyFail

 override def filter(predicate: A => Boolean): SundaySet[A] = new PropertyBasedSet[A](x => predicate(x) && property(x))

 override def flatMap[B](f: A => SundaySet[B]): SundaySet[B] = politelyFail

 override def foreach(f: A => Unit): Unit = politelyFail

 override def unary_! : SundaySet[A] = new PropertyBasedSet[A](x => !property(x))

 override def apply(element: A): Boolean = property(element)

 def politelyFail = throw new IllegalArgumentException("Really Deep Rabbit hole!")
}

class NonEmpty[A](val head : A, val tail : SundaySet[A]) extends SundaySet[A] {
 override def contains(element: A): Boolean = {
   head == element || this.tail.contains(element)
 }

 override def --(other: SundaySet[A]): SundaySet[A] =
  if (other.contains(head)){
    tail.--(other - head)
  } else {
    tail.--(other) + head
  }

 override def &(other: SundaySet[A]): SundaySet[A] = filter(other)

 override def -(element: A): SundaySet[A] = if (head == element) tail else (tail - element) + head

 override def +(element: A): SundaySet[A] = {
   if (this.contains(element)) this else new NonEmpty[A](element, this)
 }

  override def ++(other: SundaySet[A]): SundaySet[A] = {
   tail ++ other + head
 }

 override def map[B](f: A => B): SundaySet[B] = tail.map(f)+ f(head)

 override def filter(predicate: A => Boolean): SundaySet[A] = {
  if (predicate(head)){
    tail.filter(predicate) + head
  } else {
    tail.filter(predicate)
  }
 }

 override def apply(v1: A): Boolean = this.contains(v1)

 override def flatMap[B](f: A => SundaySet[B]): SundaySet[B] = {
   tail.flatMap(f) ++ f(head)
 }

 override def foreach(f: A => Unit) = {
   f(head)
   tail foreach(f)
 }

 override def unary_! : SundaySet[A] = new PropertyBasedSet[A](x => !this.contains(x))
}

object SundaySet {
 def apply[A](values : A*): SundaySet[A] = {
  @tailrec
  def build(valuesSeq: Seq[A], acc : SundaySet[A]): SundaySet[A] = {
    if (valuesSeq.isEmpty)
      acc
    else
      build(valuesSeq.tail, acc + valuesSeq.head)
  }
   build(values, new Empty[A])
 }
}

object SundaySetPlayGround extends App{
  val kaycee = SundaySet(1,2,2,2,3,4,5,6,7)
 // kaycee map(_*3) foreach println

 val even = SundaySet(2,4,6,8)
 val odd = SundaySet(1,3,5,7)
 val nature = SundaySet(1,2,3,4,5,6,7,8,9)

 println("nature intersect even")
 nature & even foreach println

 println("even diff odd")
 even -- odd foreach println

 val notSingleDigitNatureNumbers = !nature

 println(notSingleDigitNatureNumbers(10)) // true
 println(notSingleDigitNatureNumbers(6)) // false
 println(notSingleDigitNatureNumbers(2)) // false

 val notSingleDigitNatureNumbersIncludingSix = notSingleDigitNatureNumbers + 6

 println(notSingleDigitNatureNumbersIncludingSix(6)) //true
}