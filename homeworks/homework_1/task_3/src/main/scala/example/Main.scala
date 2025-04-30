package example

object Main extends App {
  val text = " Scala! This is "
  val name = "Kolbin Nikita"

  val enGreetings = "Hello"
  val esGreetings = "Hola"
  val deGreetings = "Guten tag"

  def greetings(gr: String, text: String, name: String): Unit = {
    println(gr + text + name)
  }

  greetings(enGreetings, text, name)
  greetings(esGreetings, text, name)
  greetings(deGreetings, text, name)

  println()

  greetings(enGreetings, text, name.reverse)
  greetings(esGreetings, text, name.reverse)
  greetings(deGreetings, text, name.reverse)
}
