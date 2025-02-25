val my_name = "Marina Abrosimova"
val reverse_name = my_name.reverse
val message = " Scala! This is "
val list = List("Hola", "Nazdar", "Salut")

def messages(list: List[String]): Unit = {
  list.foreach(item => println(item + " Scala! This is " + my_name))
}

def reverse_messages(list: List[String]): Unit = {
  list.foreach(item => println(item + " Scala! This is " + reverse_name))
}

@main def run() =
  messages(list)
  reverse_messages(list)
