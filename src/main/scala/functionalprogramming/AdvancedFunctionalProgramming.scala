package functionalprogramming

object AdvancedFunctionalProgramming extends App {
  val pf1 : PartialFunction[Int, String] = {
    case 1 => "First"
    case 2 => "Second"
  }

  val tp = pf1.lift // make pf1 a total function

  println(tp(2))
  println(tp(3))

  val chatBot : PartialFunction[String, String] = {
    case "Hi" => "Hello"
    case "Goodbye" => "Byte"
    case "Thank you" => "You're welcome"
    case "Sorry" => "It's ok"
    case _ => "Glad to hear that"
  }
  scala.io.Source.stdin.getLines().map(chatBot).foreach(println)
}
