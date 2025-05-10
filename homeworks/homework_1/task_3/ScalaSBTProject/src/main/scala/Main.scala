object Main extends App {
  val languages = Seq("Hello", "Hola", "Guten tag")
  val name = "Artem Bachinin"
  val reversedName = name.reverse

  def printGreetings(nameVariant: String): Unit = {
    languages.foreach { lang =>
      println(s"$lang Scala! This is $nameVariant")
    }
  }

  printGreetings(name)
  printGreetings(reversedName)
}