package example

object Main extends App {
  def greeting(g: Array[String], n: Array[String]): Unit = {
    for (i <- 0 to (n.length - 1)) {
      for (j <- 0 to (g.length - 1)) {
        println(g(j) + " Scala! This is " + n(i))
      }
    }
  }

  var names = Array("Daniil Veretennikov", "kovinnetereV liinaD")
  var greetings = Array("Hello", "Hola", "Guten tag")
  greeting(greetings, names)
}
