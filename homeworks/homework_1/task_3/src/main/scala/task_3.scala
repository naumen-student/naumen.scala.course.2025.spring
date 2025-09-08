object Main extends App {
  val name = "Max Telepov"
  val greetings = List("Привет", "Hola", "Guten tag")

  def printGreeting(greeting: String, name: String): Unit = {
    val message = s"$greeting, Scala! Это $name"
    println(message)

    val reversedName = name.reverse
    val reversedMessage = s"$greeting, Scala! Это $reversedName"
    println(reversedMessage)
  }

  greetings.foreach(greet => printGreeting(greet, name))
}
