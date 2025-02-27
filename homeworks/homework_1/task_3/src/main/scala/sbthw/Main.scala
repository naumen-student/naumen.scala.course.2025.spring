object Main extends App {
  val name = "Artem Sokolov"
  val greetings = List("Hello", "Hola", "Guten tag")

  def printGreeting(greeting: String, name: String): Unit =
    println(s"$greeting Scala! This is $name")

  greetings.foreach(printGreeting(_, name))

  val reversedName = name.reverse
  greetings.foreach(printGreeting(_, reversedName))
}
