package example

object Main extends App {
  def formatGreetings(name: String, greeting: String = "Hello") =
    println(s"$greeting Scala! This is $name")

  val myName = "Евгений"

  // 1
  formatGreetings(myName)

  // 2
  formatGreetings(myName, "Guten tag")

  List("Hello", "Hola", "Guten tag")
    .foreach(formatGreetings(myName, _))
}
