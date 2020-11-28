package functionalprogramming

object Monad extends App {
 val s = Attempt[Int] {
   val n = 1
   n + 3
 }

  println(s)
  val lazyMonad = LazyMonad{
    println("today is Monday")
    10
  }

  val lz2 = lazyMonad.flatMap(n => {
    LazyMonad {
     n
    }
  })
  lz2.get

//  println(lz2.get)

}


trait Attempt[+A] {
  def flatMap[B](a : A => Attempt[B]) : Attempt[B]
}

object Attempt {
  def apply[A](a : => A): Attempt[A] = {
    try {
      Success(a)
    } catch {
      case e : Exception => Fail(e)
    }
  }
}

case class Success[+A](value : A) extends Attempt[A]{
  override def flatMap[B](f: A => Attempt[B]): Attempt[B] = {
    try {
      f(value)
    } catch {
      case e : Exception => Fail(e)
    }
  }
}

case class Fail(e: Exception) extends Attempt[Nothing] {
  override def flatMap[B](a: Nothing => Attempt[B]): Attempt[B] = this
}

/***
 * left identity :
 *
 * unit.flatMap(f) = f
 * LazyMonad(x)
 * @param f
 * @tparam T
 */
class LazyMonad[+T](f: => T){
  def flatMap[U](fz : (=>T) => LazyMonad[U]) : LazyMonad[U] = fz(this.value) // we don't use fz(f) because it will cause f is called multiple times in some cases. The solution is 'call by need'
  lazy val value : T = f // call by need !
  def get : T = value
}
object LazyMonad {
  def apply[T](f : => T): LazyMonad[T] = new LazyMonad[T](f)
}


