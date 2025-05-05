val name = "Anna Rebak"
var reversed_name = name.reverse
val greeting = List("Hello")
val list = List("Привет", "Hi", "Bonjour")

def printGreetings(greetings: List[String], name: String): Unit = {
  greetings.foreach { i => println(s"$i Scala! This is $name")}
}

printGreetings(greeting, name)
printGreetings(list, name)
printGreetings(list, reversed_name)