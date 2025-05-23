import scala.annotation.tailrec
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
    // Перебираем все пары индексов (i, j), где i != j, затем выбираем те, что удовлетворяют условию,
    // и возвращаем последний найденный результат (или (-1,-1), если пара не найдена).
    items.indices
      .flatMap(i => items.indices.filter(_ != i).map(j => (i, j)))
      .filter { case (i, j) => items(i) + items(j) == sumValue }
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
      def loop(i: Int, acc: Int): Int = {
        if (i < 0) acc
        else {
         val index = i + 1
         val head = items(i)
         val newAcc = (if (head % 2 == 0) head * acc else -head * acc) + index
         loop(i - 1, newAcc)
       }
      }
      loop(items.length - 1, 1)
    }


    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
    @tailrec
    def loop(low: Int, high: Int): Option[Int] = {
      if (low > high) None
      else {
        val mid = low + (high - low) / 2
        if (items(mid) == value) Some(mid)
        else if (items(mid) < value) loop(mid + 1, high)
        else loop(low, mid - 1)
      }
    }
    if (items.isEmpty) None else loop(0, items.length - 1)
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

    def toName(n: Int): String = {
      // Функция для преобразования числа в список цифр в системе счисления с основанием 26.
      // Для n < 26 возвращает одно число, для n >= 26 – вычисляет "разряды".
      def convert(x: Int): List[Int] = {
        if (x < 26) List(x)
        else convert(x / 26 - 1) :+ (x % 26)
      }
      val digits = convert(n)
      val first = ('A' + digits.head).toChar.toString
      val rest = digits.tail.map(d => ('a' + d).toChar).mkString
      first + rest
    }

    (0 until namesCount).toList.map(toName)
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

  // Безопасный сервис для работы с телефонными номерами.
  class PhoneServiceSafety(unsafePhoneService: SimplePhoneService) {

    // Если метод findPhoneNumber вернёт null, то возвращается None, иначе Some(phone)
    def findPhoneNumberSafe(num: String): Option[String] =
      Option(unsafePhoneService.findPhoneNumber(num))

    // Безопасное добавление номера: в случае успешного добавления возвращается Right(()),
    // а в случае возникновения исключения – Left с текстом ошибки.
    def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
      try {
        unsafePhoneService.addPhoneToBase(phone)
        Right(())
      } catch {
        case e: Exception => Left(e.getMessage)
      }
    }

    // Безопасное удаление номера: возвращает Right(()) при успехе или Left с текстом ошибки
    def deletePhoneSafe(phone: String): Either[String, Unit] = {
      try {
        unsafePhoneService.deletePhone(phone)
        Right(())
      } catch {
        case e: Exception => Left(e.getMessage)
      }
    }
  }

  // Безопасная версия сервиса смены номера, использующая PhoneServiceSafety.
  class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      // Пытаемся найти старый номер, если найден – безопасно удаляем
      val deletionResult: Either[String, Unit] = phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
        case Some(existingPhone) => phoneServiceSafety.deletePhoneSafe(existingPhone)
        case None                => Right(()) // Если старый номер не найден, ничего удалять не нужно
      }

      deletionResult match {
        case Left(error) => error  // Если при удалении произошла ошибка – возвращаем её
        case Right(_) =>
          // Пытаемся добавить новый номер
          phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
            case Left(error) => error
            case Right(_)    => "ok"
          }
      }
    }
  }
}