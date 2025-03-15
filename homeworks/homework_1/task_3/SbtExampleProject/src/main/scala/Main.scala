object Main extends App {
  val name = "Ilya Glukhov"

  def printGreet(greeting: String, name: String): Unit =
    println(s"$greeting, Scala! This is $name.")

  val greetings = Seq("Hello", "Hola", "Guten Tag")

  greetings.foreach(printGreet(_, name))

  val reversedName = name.reverse
  greetings.foreach(printGreet(_, reversedName))
}
