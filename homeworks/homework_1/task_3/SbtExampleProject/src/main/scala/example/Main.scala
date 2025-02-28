package example

@main def run(): Unit =
  val name = "Sazhin Egor"
  val greeting = s"Hello Scala! This is $name"
  println(greeting)

  val greetings = List("Hola", "Guten Tag", "Bonjur")
  for (gr <- greetings) println(s"$gr Scala! This is $name")

  val reversedName = name.reverse
  for (gr <- greetings) println(s"$gr Scala! This is $reversedName")
