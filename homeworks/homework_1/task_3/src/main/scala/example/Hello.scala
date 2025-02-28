object Main extends App {
  val name = "Максим Шарафутдинов "
  val reversedName = name.reverse
  val greetings = List("Hello", "Hola", "Guten tag")
  val baseMessage = "Scala! This is"
  def printMessage(greeting: String, name: String): Unit = {
    println(s"$greeting $baseMessage $name")
  }
  greetings.foreach(greeting => printMessage(greeting, name))
  greetings.foreach(greeting => printMessage(greeting, reversedName))
}