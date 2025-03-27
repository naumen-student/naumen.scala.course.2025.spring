import scala.annotation.tailrec
import scala.concurrent.duration.Duration.Inf.+
import scala.util.Random

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

  def findSumFunctional(items: List[Int], sumValue: Int): (Int, Int) =
    items.zipWithIndex.combinations(2).collectFirst {
        case List((a, j), (b, i)) if a + b == sumValue => (i, j)
    }.getOrElse((-1, -1))


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

  def tailRecRecursion(items: List[Int], index: Int = 1): Int = {
    @tailrec
    def loop(items: List[Int], index: Int, acc: Int): Int = {
      items match {
        case head :: tail =>
          val newValue = head % 2 match {
            case 0 => head * acc + index
            case _ => -1 * head * acc + index
          }
          loop(tail, index - 1, newValue)
        case _ => acc
      }
    }
    loop(items.reverse, items.size, 1)
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
      if (low > high) {
        None
      } else {
        val mid = low + (high - low) / 2
        items(mid) match {
          case x if x == value => Some(mid)
          case x if x < value  => search(mid + 1, high)
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

  def generateNames(namesCount: Int, random: Random = new Random()): List[String] = {
    val letters = ('A' to 'Z') ++ ('a' to 'z')

    def generateName(length: Int): String = {
      (1 to length).map {
        case 1 => letters(random.nextInt(26))
        case _ => letters(26 + random.nextInt(26))
      }.mkString
    }
    List.fill(namesCount)(generateName(random.nextInt(10) + 3))
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
    def findPhoneNumberSafe(num: String): Either[String, String] = {
      Option(unsafePhoneService.findPhoneNumber(num)) match {
        case Some(phone) => Right(phone)
        case None => Left("Phone number not found")
      }
    }

    def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
      try {
        unsafePhoneService.addPhoneToBase(phone)
        Right(())
      } catch {
        case _: InternalError => Left("Invalid phone string")
      }
    }

    def deletePhoneSafe(phone: String): Either[String, Unit] = {
      try {
        unsafePhoneService.deletePhone(phone)
        Right(())
      } catch {
        case _: Exception => Left("Error deleting phone")
      }
    }
  }

  class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      val oldPhoneResult = phoneServiceSafety.findPhoneNumberSafe(oldPhone)

      oldPhoneResult match {
        case Right(oldPhoneRecord) =>
          phoneServiceSafety.deletePhoneSafe(oldPhoneRecord) match {
            case Right(_) =>
              phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                case Right(_) => "ok"
                case Left(error) => error
              }
            case Left(error) => error
          }
        case Left(error) => error
      }
    }
  }
}