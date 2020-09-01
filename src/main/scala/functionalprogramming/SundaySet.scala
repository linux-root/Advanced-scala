package functionalprogramming

import scala.annotation.tailrec

trait SundaySet[A] extends (A => Boolean){
 def contains(element: A) : Boolean
 def +(element:A) : SundaySet[A]
 def ++(other: SundaySet[A]) : SundaySet[A]
 def map[B](f : A => B) : SundaySet[B]
 def filter(predicate : A => Boolean) : SundaySet[A]
 def flatMap[B](f : A => SundaySet[B]) : SundaySet[B]
 def foreach(f : A => Unit)
}

class Empty[A] extends SundaySet[A] {
 override def contains(element: A): Boolean = false

 override def +(element: A): SundaySet[A] = new NonEmpty[A](element, this)

 override def ++(other: SundaySet[A]): SundaySet[A] = other

 override def map[B](f: A => B): SundaySet[B] = new Empty[B]

 override def filter(predicate: A => Boolean): SundaySet[A] = new Empty[A]

 override def apply(v1: A): Boolean = this.contains(v1)

 override def flatMap[B](f: A => SundaySet[B]): SundaySet[B] = new Empty[B]

 override def foreach(f: A => Unit): Unit = ()
}

class NonEmpty[A](val head : A, val tail : SundaySet[A]) extends SundaySet[A] {
 override def contains(element: A): Boolean = {
   head == element || this.tail.contains(element)
 }

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
  kaycee map(_*3) foreach println
}