object Main extends App {

  def sayHello(greeting: String, name: String) = println(s"$greeting Scala! This is $name")

  val name = "Kirill Yegorov"
  val greetings = List("Hello", "Cześć", "Hi")

  for (greeting <- greetings) sayHello(greeting, name)
  for (greeting <- greetings) sayHello(greeting, name.reverse)

}