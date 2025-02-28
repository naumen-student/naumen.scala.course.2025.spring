package example

object Main extends App {
  val name = "Elena Kleshcheva";

  val greeting = " Scala! This is ";

  val helloWords = Map(
    "EN" -> "Hello",
    "GE" -> "Guten tag"
  )

  def hello(lsid: String, name: String): Unit = {
    println(helloWords(lsid) + greeting + name);
  }

  hello("EN", name);
  hello("GE", name);
  hello("EN", name.reverse);
  hello("GE", name.reverse);
}

