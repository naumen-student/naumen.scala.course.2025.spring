package main.scala

object Main extends App {
  var name = "Violetta Vizner"
  var greetingStart = "Hello"
  def greeting = s"$greetingStart, Scala! This is "
  def fullGreeting: String = greeting + name

  println(fullGreeting)

  greetingStart = "Hi"

  println(fullGreeting)

  greetingStart = "Privet"

  println(fullGreeting)

  name = "renziV atteloiV"

  println(fullGreeting)

  greetingStart = "Hi"

  println(fullGreeting)
}