object Main extends App {
  val name1 = "Tenkacheva Daria Andreevna"
  val name2 = "avehcakneT airaD anveerdnA"

  val greetings = List("Hello", "Hola", "Guten tag")

  def printGreetings(greetings: List[String], name: String): Unit = {
    greetings.foreach(hi => println(s"$hi, Scala! This is $name"))
  }

  println("Part 1")
  printGreetings(greetings, name1)
  println()
  printGreetings(greetings, name2)

  println("")

  val a = s", Scala! This is $name1"
  val b = s", Scala! This is $name2"

  println("Part 2")
  greetings.foreach(hi => println(s"$hi$a"))
  println()
  greetings.foreach(hi => println(s"$hi$b"))
}