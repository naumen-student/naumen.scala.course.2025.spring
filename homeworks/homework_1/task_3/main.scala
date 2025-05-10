@main
def main(): Unit = {
  def greetWithName(name: String): Unit = {
    val greetings = List("Hello", "Halo", "Привет")

    // Выводим имя с приветствиями
    for (g <- greetings) {
      println(s"$g Scala! This is $name")
    }

    // Выводим имя с перевернутым именем для каждого приветствия
    for (g <- greetings) {
      println(s"$g Scala! This is ${name.reverse}")
    }
  }

  // Вызов функции с примером имени
  greetWithName("Anton Novikov")
}

