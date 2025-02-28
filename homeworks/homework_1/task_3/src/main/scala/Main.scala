object Main extends App {
  val name = "Chechulin Anton"
  private val reversedName = name.reverse
  private val greetingWords = List("Hola", "Guten tag", "Salut")

  private val printGreeting = (greeting: String, name: String) => println(s"$greeting Scala! This is $name")

  printGreeting("Hello", "Anton")

  greetingWords.foreach(g => printGreeting(g, name))

  greetingWords.foreach(g => printGreeting(g, reversedName))
}