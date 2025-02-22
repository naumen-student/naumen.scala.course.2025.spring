package naumen

object Main extends App {
  val greetings = Seq("Hello", "Hola", "Guten tag")

  val name = "Ivan Gorbunov"
  greetings.foreach { greeting =>
    greet(greeting, name)
  }

  val reversedName = name.reverse
  greetings.foreach { greeting =>
    greet(greeting, reversedName)
  }

  def greet(greeting: String, name: String): Unit = {
    println(s"$greeting Scala! This is $name")
  }
}

