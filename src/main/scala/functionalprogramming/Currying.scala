package functionalprogramming

object Currying extends App {

val printNumber : String => Number => Unit = fm => number => println(fm.format(number))
val printNumberFormat1 = printNumber("%3.2f") // "%<Number of indention characters>.<number of digits after floating point>f"
val printNumberFormat2 = printNumber("%8.4f")

  printNumberFormat1(Math.PI)

  def byName(n : => Int) : Int = {
    println("call byName")
    n + n + 98
  }

  def byValue(n : Int): Int = {
    println("call byValue")
    n + n + 98
  }

  def byFunction (n : () => Int) = {
    println("call by Function")
    n() + n() + 98 // apply to method1 is invalid because it doesn't exist expression method1()
  }

  def method1: Int = {
    // this method cannot be passed as a function value because its type is : Int, not () => Int
    println("call method1")
    1 + 1
  }

  def method2(): Int = {
    // compare to method 1, method2 type is () => Int so that can be passed as function
    println("call method2")
    1 + 1
  }

  byName(method2()) // IMPORTANT : parameter of call by name is NOT a function type
  println("---------")
  byValue(method1)
  println("---------")
//  byFunction(method1) // illegal call. Compiler cannot convert method1 to function
  byFunction(method2) // legal call. Compiler can convert method2 to  function automatically
}
