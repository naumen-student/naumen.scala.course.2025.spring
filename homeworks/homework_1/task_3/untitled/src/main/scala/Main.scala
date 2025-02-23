object Main extends App {
  def printGreeting(greeting: String, name: String): Unit = {
    println(s"$greeting Scala! This is $name")
  }

  def printGreetings(greetings: List[String], name: String): Unit = {
    for (greeting <- greetings) {
      printGreeting(greeting, name)
    }
  }

  val name = "Nikita Azanov"
  val reversedName = name.reverse
  val greetings = List("Hello", "Hola", "Guten tag")

  printGreetings(greetings, name)
  printGreetings(greetings, reversedName)
}