object Main extends App {
  val base = "%s Scala! This is %s"
  
  val name = "Sazhin Egor"
  
  val greetings = List("Hello", "Hola", "Guten Tag")
  
  def printMessage(greeting: String, name: String): Unit = {
    val message = base.format(greeting, name)
    println(message)
  }
  
  greetings.foreach { greeting =>
    printMessage(greeting, name)           
    printMessage(greeting, name.reverse) 
  }
}