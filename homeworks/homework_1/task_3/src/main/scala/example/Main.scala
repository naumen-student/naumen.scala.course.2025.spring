package example

object Main extends App {
  val name = "Maxim Finadeev"
  val greetings = List("Hello", "Hola", "Guten tag")

  def printGreetings(name: String): Unit = {
    greetings.foreach(g => println(s"$g Scala! This is $name"))
  }

  printGreetings(name)

  val reversedName = name.reverse
  printGreetings(reversedName)
}