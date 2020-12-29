package implycit

object JsonSerielizer extends App{

  case class User(name : String, age : Int)
  case class Post(title : String, content : String, author : User)


  sealed trait JsonValue {
    def stringify : String
  }

  final case class JsonString(value : String) extends JsonValue{
    override def stringify: String = "\"" + value + "\""
  }

  final case class JsonNumber(value : Int) extends JsonValue{
    override def stringify: String = value.toString
  }

  final case class JsonObject(values : Map[String, JsonValue]) extends JsonValue{
    override def stringify: String = values.map{
      case (key, value ) => "\"" + key + "\":" + value.stringify
    }.mkString("{", ",", "}")
  }

  final case class JsonArray(values : List[JsonValue]) extends JsonValue{
    override def stringify: String = values.map(_.stringify).mkString("[", ",", "]")
  }

  trait JsonConverter[T]{
    def convert(value : T) : JsonValue
  }

  implicit object StringConverter extends JsonConverter[String]{
    override def convert(value: String): JsonValue = JsonString(value)
  }

  implicit object UserConverter extends JsonConverter[User]{
    override def convert(user: User): JsonValue = JsonObject(Map(
      "name" -> JsonString(user.name),
      "age" -> JsonNumber(user.age))
    )
  }

  implicit object PostConverter extends JsonConverter[Post]{
    override def convert(post: Post): JsonValue = JsonObject(Map(
      "title" ->  post.title.toJson,
      "content" -> post.content.toJson,
      "author" -> post.author.toJson
    ))
  }

  implicit class JsonOps[T](value : T){
    def toJson(implicit converter: JsonConverter[T]) : JsonValue = converter.convert(value)
  }


  val watson = User("Watson", 22)
  println(watson.toJson.stringify)

  val post = Post("Advanced scala", "Scala is awesome", watson)

  println(post.toJson.stringify)
}
