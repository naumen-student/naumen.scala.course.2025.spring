
object Exercises {

  /**
   * Задание №1
   * Дана императивная функция findSumImperative.
   * Напишите ее аналог (findSumFunctional) в функциональном стиле.
   *
   * ПОДСКАЗКА
   * Стоит воспользоваться методами, которые предоставляет объект List или рекурсией.
   * Страница с полезностями List: https://alvinalexander.com/scala/list-class-methods-examples-syntax/
   */
  def findSumImperative(items: List[Int], sumValue: Int): (Int, Int) = {
    var result: (Int, Int) = (-1, -1)
    for (i <- 0 until items.length) {
      for (j <- 0 until items.length) {
        if (items(i) + items(j) == sumValue && i != j) {
          result = (i, j)
        }
      }
    }
    result
  }

  def findSumFunctional(items: List[Int], sumValue: Int): (Int, Int) = {
    items.indices.flatMap { i =>
      items.indices.collect {
        case j if i != j && items(i) + items(j) == sumValue => (i, j)
      }
    }.headOption.getOrElse((-1, -1))
  }


  /**
   * Задание №2
   *
   * Дана рекурсивная функция simpleRecursion.
   * Перепишите ее так, чтобы получилась хвостовая рекурсивная функция.
   *
   * Для прохождения теста на большое количество элементов в списке
   * используйте анотацию @tailrec к вашей функции.
   */
  def simpleRecursion(items: List[Int], index: Int = 1): Int = {
    items match {
      case head :: tail =>
        if (head % 2 == 0) {
          head * simpleRecursion(tail, index + 1) + index
        } else {
          -1 * head * simpleRecursion(tail, index + 1) + index
        }
      case _ => 1
    }
  }

  def tailRecRecursion(items: List[Int]): Int = {
    @tailrec
    def helper(remaining: List[Int], index: Int, acc: Int): Int = {
      remaining match {
        case head :: tail =>
          if (head % 2 == 0) {
            helper(tail, index + 1, head * acc + index)
          } else {
            helper(tail, index + 1, -1 * head * acc + index)
          }
        case Nil => acc
      }
    }

    helper(items, 1, 1)
  }

  /**
   * Задание №3
   * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
   * Необходимо возвращать индекс соответствующего элемента в массиве
   * Если ответ найден, то возвращается Some(index), если нет, то None
   */

  def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
    @annotation.tailrec
    def search(low: Int, high: Int): Option[Int] = {
      if (low > high) None
      else {
        val mid = low + (high - low) / 2
        items(mid) match {
          case m if m == value => Some(mid)
          case m if m < value => search(mid + 1, high)
          case _ => search(low, mid - 1)
        }
      }
    }

    search(0, items.length - 1)
  }

  /**
   * Задание №4
   * Реализуйте функцию, которая генерирует список заданной длинны c именами.
   * Функция должна соответствовать всем правилам функционального программирования.
   *
   * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
   */

  def generateNames(namesCount: Int): List[String] = {
    if (namesCount < 0) throw new Throwable("Invalid namesCount")

    val letters = ('A' to 'Z') ++ ('А' to 'Я')
    val lowerLetters = ('a' to 'z') ++ ('а' to 'я')

    def randomName: String = {
      val firstChar = letters(Random.nextInt(letters.length))
      val rest = (1 to Random.nextInt(9) + 1).map(_ => lowerLetters(Random.nextInt(lowerLetters.length))).mkString
      firstChar + rest
    }

    def generate(n: Int, acc: Set[String] = Set.empty): List[String] = {
      if (n <= 0) acc.toList
      else {
        val name = randomName
        if (acc.contains(name)) generate(n, acc)
        else generate(n - 1, acc + name)
      }
    }

    generate(namesCount)
  }

}

/**
 * Задание №5
 *
 * Дана реализация сервиса по смене номера SimpleChangePhoneService с методом changePhone
 * Необходимо написать реализацию этого сервиса с учетом правил работы со сторонними эффектами (SideEffects).
 *
 * Для этого необходимо сначала реализовать собственный сервис работы с телефонными номерами (PhoneServiceSafety),
 * используя при этом методы из unsafePhoneService.
 * Методы должны быть безопасными, поэтому тип возвращаемых значений необходимо определить самостоятельно.
 * Рекомендуется воспользоваться стандартными типами Scala (например Option или Either).
 *
 * Затем, с использованием нового сервиса, необходимо реализовать "безопасную" версию функции changePhone.
 * Функция должна возвращать ok в случае успешного завершения или текст ошибки.
 *
 * Изменять методы внутри SimplePhoneService не разрешается.
 */

object SideEffectExercise {

  import Utils._

  class SimpleChangePhoneService(phoneService: SimplePhoneService) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      val oldPhoneRecord = phoneService.findPhoneNumber(oldPhone)
      if (oldPhoneRecord != null) {
        phoneService.deletePhone(oldPhoneRecord)
      }
      phoneService.addPhoneToBase(newPhone)
      "ok"
    }
  }


  class PhoneServiceSafety(unsafePhoneService: SimplePhoneService) {
    def findPhoneNumberSafe(num: String): Option[String] = {
      unsafePhoneService.findPhoneNumber(num) match {
        case null => None
        case n => Some(n)

          def addPhoneToBaseSafe(phone: String): Either[String, Unit] =
            Try(unsafePhoneService.addPhoneToBase(phone)) match {
              case Success(ok) => Right(ok)
              case Failure(exception) => Left(exception.getMessage)
            }

          def deletePhone(phone: String): Unit = unsafePhoneService.deletePhone(phone)
      }

      class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
          phoneServiceSafety.findPhoneNumberSafe(oldPhone).foreach(phoneServiceSafety.deletePhone)
          phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
            case Right(_) => "ok"
            case Left(message) => message
          }
        }
      }
    }
