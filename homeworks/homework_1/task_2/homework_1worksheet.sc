val name = "Lev Chernyugov"
val reversedName = name.reverse

def printGreeting(greeting: String): Unit = {
  println(s"$greeting Scala! This is $name")
  println(s"$greeting Scala! This is $reversedName")
}

val greetings = List("Hello", "Hola", "Guten tag")
greetings.foreach(printGreeting)