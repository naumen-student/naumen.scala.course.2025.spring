package Example

object Main extends App
{
  def printAllGreetings(greetings: Array[String], names: Array[String]): Unit = {
    for (i <- 0 until names.length) {
      for (j <- 0 until greetings.length) {
        printGreetings(greetings(j), names(i))
      }
      println()
    }
  }

  def printGreetings(greetingsWords: String, name: String): Unit = {
    println(greetingsWords + " Scala! This is " + name)
  }

  val name = "Alchin Ivan"
  val names = Array(name, name.reverse)

  var greetings = Array("Hello", "Hola", "Guten tag")

  printAllGreetings(greetings, names)
}
