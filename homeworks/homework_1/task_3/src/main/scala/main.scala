object main extends App {
  def greeting(greet: Array[String], name: Array[String]): Unit = {
    for (i <- 0 to (name.length - 1)) {
      for (j <- 0 to (greet.length - 1)) {
        println(greet(j) + " Scala! This is " + name(i))
      }
    }
  }

  val name = "Marina Apkaeva"
  val names = Array(name, name.reverse)
  val greetings = Array("Hello", "Hola", "Guten tag")

  greeting(greetings, names)

}