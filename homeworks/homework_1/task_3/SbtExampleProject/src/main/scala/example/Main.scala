@main def run() =
  val name = "Nikita Yasenev"
  val greeting = s"Hello Scala! This is $name"
  println(greeting)

  val greetings = List("Hola", "Guten tag", "Bonjour", "Ciao", "Ni hao", "Konnichiwa")

  val foreignGreetings = greetings.map(g => s"$g Scala! This is $name")

  val reversedName = name.reverse

  val reversedGreetings = greetings.map(g => s"$g Scala! This is $reversedName")
  reversedGreetings.foreach(println)