package functionalprogramming

object LazyValuation extends App{

  val mondayStream = SundayStream.from(1)((n : Int) => n + 1)

  val conCat = mondayStream.take(10) ++ mondayStream.take(10)
  mondayStream.take(10).flatMap(e => new Cons[Int](e*2, new Cons(e*3, EmptyStream))).foreach(println)
}

abstract class SundayStream[+A] {
  def isEmpty : Boolean
  def head : A
  def tail : SundayStream[A]
  def #::[B >: A](elem: B) : SundayStream[B] // prepend
  def ++[B >: A](anotherStream: => SundayStream[B]) : SundayStream[B]
  def foreach(f : A => Unit )
  def filter(f: A => Boolean) : SundayStream[A]
  def map[B](f : A => B) : SundayStream[B]
  def flatMap[B](f: A => SundayStream[B]) : SundayStream[B]
  def take(n : Int) : SundayStream[A]
  def takeAsList(n: Int): List[A]
}

object EmptyStream extends SundayStream[Nothing] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new IllegalArgumentException("call EmptyStream.head")

  override def tail: SundayStream[Nothing] = throw new IllegalArgumentException("call EmptyStream.tail")

  override def #::[B >: Nothing](elem: B): SundayStream[B] = new Cons[B](elem, this)

  override def ++[B >: Nothing](anotherStream: => SundayStream[B]): SundayStream[B] = anotherStream

  override def foreach(f: Nothing => Unit): Unit = ()

  override def map[B](f: Nothing => B): SundayStream[B] = this

  override def flatMap[B](f: Nothing => SundayStream[B]): SundayStream[B] = EmptyStream

  override def take(n: Int): SundayStream[Nothing] = this

  override def takeAsList(n: Int): List[Nothing] = Nil

  override def filter(f: Nothing => Boolean): SundayStream[Nothing] = this
}

class Cons[A](override val head : A, lazyTail : => SundayStream[A]) extends SundayStream[A]{
  override def isEmpty: Boolean = false

  override lazy val tail: SundayStream[A] = lazyTail

  override def #::[B >: A](elem: B): SundayStream[B] = new Cons(elem, this)

  // this causes  StackOverFlowException because  another stream isn't called by name
//  override def ++[B >: A](anotherStream: SundayStream[B]): SundayStream[B] = this.head #:: this.tail ++ anotherStream


  override def ++[B >: A](anotherStream: => SundayStream[B]): SundayStream[B] = {
    /* this.head #:: this.tail ++ anotherStream   => this will cause StackOverFlowEx because anotherStream will be evaluated
        as Scala Expression Evaluation. In other words, anotherStream will be reduced(called) before input to ++ operator
     */
//    (this.head #:: this.tail) ++ anotherStream // won't work
    this.head #:: this.tail.++(anotherStream) // works
    this.head #:: (this.tail ++ anotherStream) // works
  }



  override def foreach(f: A => Unit): Unit = {
    f(this.head)
    this.tail.foreach(f)
  }

  override def map[B](f: A => B): SundayStream[B] = new Cons(f(this.head), this.tail.map(f))

  override def flatMap[B](f: A => SundayStream[B]): SundayStream[B] = f(this.head) ++ this.tail.flatMap(f)

  override def take(n: Int): SundayStream[A] = {
    if (n == 0) {
      EmptyStream
    } else {
      this.head #:: this.tail.take(n - 1)
    }
  }

  override def takeAsList(n: Int): List[A] = {
    if (n == 0) {
      Nil
    } else {
      this.head :: this.tail.takeAsList(n - 1)
    }
  }

  override def filter(f: A => Boolean): SundayStream[A] = {
    if (f(this.head)) {
      this.head #:: this.tail.filter(f)
    } else {
      this.tail.filter(f)
    }
  }
}


object SundayStream {
  def from[A](start : A)(generator: A => A) : SundayStream[A] = {
     lazy val next = generator(start)
     new Cons[A](start, SundayStream.from(next)(generator))
  }
}
