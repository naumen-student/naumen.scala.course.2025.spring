object main extends App {
  var name = "Anastasiya Bobina"
  private val greetings = Seq("Hello", "Guten tag")

  private def greetInOneLanguage(greeting: String, name: String): Unit = {
    println(s"$greeting Scala! This is $name")
  }
  println("First task:")
  greetInOneLanguage(greetings.head, name)
  println("Second task:")
  private def greetInAllLanguages(name: String): Unit = {
    greetings.foreach { greeting =>
      greetInOneLanguage(greeting, name)
    }
  }
  greetInAllLanguages(name)
  println("Third task:")
  greetInAllLanguages(name.reverse)

}