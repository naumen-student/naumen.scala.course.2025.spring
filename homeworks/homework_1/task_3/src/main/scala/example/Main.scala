package example

object Main extends App {

  val name = List("Sergey", "yegreS", "geySer")
  val surname = List("Kamensky","yksnemaK","nskyKame")
  val greetings = List("Hello", "Guten tab", "Hola")

  for {
    i <- name
    j <- surname
    k <- greetings
  } {
    println(s"$k Scala! This is $i $j")
  }

}