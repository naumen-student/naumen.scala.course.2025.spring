class Main
@main def run(): Unit =
  val name = "Belousov Dima"
  val greetings = List("Hello", "Hola", "Guten tag")

  def printMessage(greeting: String, name: String): Unit = {
    println(s"$greeting Scala! This is $name")
  }

  printMessage(greetings.head, name)

  greetings.foreach(greeting => printMessage(greeting, name))

  val reversedName = name.reverse

  greetings.foreach(greeting => printMessage(greeting, reversedName))