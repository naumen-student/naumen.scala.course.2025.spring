object Main extends App {
  val name = "Kseniya Smolyakova"
  val reverse_name = name.reverse
  val message = " Scala! This is"
  val list = List("Hello", "Guten Tag", "Saluy", "Hola")

  def full_message(list: List[String]): Unit = {
    list.foreach(item => println(item + " Scala! This is " + name))
    }

  def full_message_reverse(list: List[String]): Unit = {
    list.foreach(item => println(item + " Scala! This is " + reverse_name))
  }
  full_message(list)
  full_message_reverse(list)
}
