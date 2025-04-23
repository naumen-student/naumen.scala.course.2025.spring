object Main extends App {
  println("Part 1")
  println("Hello, Scala! This is Daniil Golovinov")
  println("Hola, Scala! This is Daniil Golovinov")
  println("Guten tag, Scala! This is Daniil Golovinov")
  println()
  println("Hello, Scala! This is vonivoloG liinaD")
  println("Hola, Scala! This is vonivoloG liinaD")
  println("Guten tag, Scala! This is vonivoloG liinaD")
  println("")
  println("")
  println("Part 2")
  val a: String = ", Scala! This is Daniil Golovinov"
  val b: String = ", Scala! This is vonivoloG liinaD"
  var hello = List("Hello", "Hola", "Guten tag")
  for (hi <- hello) println(s"$hi$a")
  println()
  for (hi <- hello) println(s"$hi$b")
}