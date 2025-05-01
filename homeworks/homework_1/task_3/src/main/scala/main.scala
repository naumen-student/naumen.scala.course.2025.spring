object main extends App {
  class Person(firstName: String, lastName: String) {
    def getFirstName: String =
      firstName

    def getLastName: String =
      lastName

    def reverse(): Person =
      Person(lastName.reverse, firstName.reverse)

    override def toString: String =
      s"$firstName $lastName"
  }

  class Greeting(greeting: String) {
    def getGreeting: String =
      greeting
  }

  def printPersonalizedGreeting(greeting: Greeting, person: Person): Unit = {
    println(greeting.getGreeting + " Scala! This is " + person.toString())
  }

  val me = Person("Ivan", "Driuk")
  val reverseMe = me.reverse()

  val englishGreeting = Greeting("Hello")
  val italianGreeting = Greeting("Ciao")
  val frenchGreeting = Greeting("Bonjour")

  printPersonalizedGreeting(englishGreeting, me)
  printPersonalizedGreeting(italianGreeting, me)
  printPersonalizedGreeting(frenchGreeting, me)
  printPersonalizedGreeting(englishGreeting, reverseMe)
  printPersonalizedGreeting(italianGreeting, reverseMe)
  printPersonalizedGreeting(frenchGreeting, reverseMe)
}