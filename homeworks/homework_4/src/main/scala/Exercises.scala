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

  def findSumFunctional(items: List[Int], sumValue: Int) = {
    items
      .zipWithIndex
      .combinations(2)
      .map(t => (t(1), t.head))
      .filter(t => t._1._1 + t._2._1 == sumValue)
      .map(t => (t._1._2, t._2._2))
      .toList
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
    @tailrec
    def rec(items: List[Int], index: Int, value: Int): Int = {
      items match {
        case head :: tail => {
          if (head % 2 == 0)
            rec(tail, index - 1, head * value + index)
          else
            rec(tail, index - 1, index - head * value)
        }
        case _ => value
      }
    }

    rec(items.reverse, items.size, 1)
  }

  /**
   * Задание №3
   * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
   * Необходимо возвращать индекс соответствующего элемента в массиве
   * Если ответ найден, то возвращается Some(index), если нет, то None
   */

  def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {

    @tailrec
    def binsearch(items: List[Int], l: Int, h: Int): Option[Int] = {
      if (l > h) return None

      val m = (l + h) / 2

      items(m) match {
        case v if v == value => Some(m)
        case v if v > value => binsearch(items, l, m - 1)
        case _ => binsearch(items, m + 1, h)
      }
    }

    binsearch(items, 0, items.length - 1)

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
    List.fill(namesCount)(Random.alphanumeric
      .filter(_.isLetter)
      .take(Random.nextInt(10) + 5)
      .mkString
      .toLowerCase
      .capitalize)
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
      }
    }


    def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
      try {
        Right(unsafePhoneService.addPhoneToBase(phone))
      } catch {
        case _: InternalError => Left("Invalid phone string")
      }
    }

    def deletePhone(phone: String): Unit = unsafePhoneService.deletePhone(phone)
  }

  class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
        case Some(_) => phoneServiceSafety.deletePhone(oldPhone)
      }

      phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
        case Left(e) => e
        case Right(_) => "ok"
      }
    }
  }
}