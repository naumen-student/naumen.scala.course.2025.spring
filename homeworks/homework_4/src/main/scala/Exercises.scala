import scala.annotation.tailrec
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

  def findSumFunctional(items: List[Int], sumValue: Int): (Int, Int) = {
    items.indices
      .flatMap(i => items.indices.collect { case j if i != j && items(i) + items(j) == sumValue => (i, j) })
      .lastOption
      .getOrElse((-1, -1))
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
    def getIndex(head: Int, acc: Int, index: Int): Int = {
      head * acc + index
    }

    @tailrec
    def sum(sumItems: List[Int], index: Int, acc: Int): Int = sumItems match {
      case Nil => acc
      case head :: tail => sum(
        tail,
        index - 1,
        if (head % 2 == 0) getIndex(head, acc, index) else getIndex(-1 * head, acc, index))
    }

    sum(items.reverse, items.length, 1)
  }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

  def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
    @tailrec
    def search(left: Int, right: Int, searchValue: Int): Option[Int] = {
      if (left > right) {
        None
      }
      else {
        val mid = (left + right) / 2
        items(mid) match {
          case x if x == searchValue => Some(mid)
          case x if x < searchValue => search(mid + 1, right, searchValue)
          case _ => search(left, mid - 1, searchValue)
        }
      }
    }

    search(0, items.length - 1, value)
  }

    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

    def generateNames(namesCount: Int): List[String] = {
      require(namesCount >= 0, "Invalid namesCount")

      val alphabet = ('a' to 'z').toList

      def generateName: String = {
        Random.shuffle(alphabet).take(Random.between(3, 10)).mkString.capitalize
      }

      List.fill(namesCount)(generateName)
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

  private class PhoneServiceSafety(unsafePhoneService: SimplePhoneService) {
    def findPhoneNumberSafe(num: String): Option[String] =
      Option(unsafePhoneService.findPhoneNumber(num))

    def addPhoneToBaseSafe(phone: String): Either[String, String] =
      try {
        unsafePhoneService.addPhoneToBase(phone)
        Right("ok")
      } catch {
        case ex: Exception => Left(s"Failed to add phone: ${ex.getMessage}")
      }

    def deletePhoneSafe(phone: String): Either[String, String] =
      try {
        unsafePhoneService.deletePhone(phone)
        Right("ok")
      } catch {
        case ex: Exception => Left(s"Failed to delete phone: ${ex.getMessage}")
      }
  }

  class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      phoneServiceSafety.findPhoneNumberSafe(oldPhone)
        .map(_ => phoneServiceSafety.deletePhoneSafe(oldPhone))
        .getOrElse(Right("ok"))
        .flatMap(_ => phoneServiceSafety.addPhoneToBaseSafe(newPhone))
        .merge
    }
  }
}
