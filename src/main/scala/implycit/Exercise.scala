package implycit

// import implycit.K.Converter // uncommented to run solution 2

import java.util.Optional

object Exercise extends App{
  val oJava1 = Optional.of(3)
  println(oJava1.asScala)
  val oJava2 = Optional.empty()
  println(oJava2.asScala)


  // solution 2 : implict as a normal method
  abstract class ToScala[+T]{ // +T for case Optional<Nothing> because for every T, Nothing is always subtype of T
    def asScala : Option[T]
  }
  implicit def noMatterTheName[T](optional : Optional[T]) : ToScala[T] = new ToScala[T] {
    override def asScala: Option[T] = if (optional.isPresent){
      Some(optional.get())
    } else {
      None
    }
  }
}


object K {
  // solution 1 : implicit as a constructor method of class
   implicit class Converter[+T](optional: Optional[T]){
      def asScala : Option[T] =
         if (optional.isPresent){
            Some(optional.get())
         }else {
            None
         }
   }
}






