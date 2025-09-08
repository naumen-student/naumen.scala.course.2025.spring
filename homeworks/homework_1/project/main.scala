object Main extends App {
  val name = "moos"
  val greeting = f"Hello Scala! This is ${name}"
  val startList = "Hello" :: "Hola" :: "Guten tag" :: Nil
  startList
    .map(greeting.replace("Hello", _))
    .foreach(println)
  startList
    .map(greeting.replace("Hello", _).replace(name, name.reverse))
    .foreach(println)
}
