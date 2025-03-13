package example

object Main extends App {
  val greetings = List("Hello ", "Hola ", "Salam ")
  val name = "Rashad Aliev"

  def greeting(greet: String, name: String): String = {
    s"$greet Scala! This is $name"
  }

  for (greet <- greetings) println(greeting(greet, name))
  for (greet <- greetings) println(greeting(greet, name.reverse))
}
