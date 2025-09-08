import scala.annotation.tailrec
import scala.util.{Random, Try}

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
          .flatMap(i => items.indices.collect(
              { case j if i != j && items(i) + items(j) == sumValue => (i, j) }))
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
        doTailRec(items.reverse, 1, items.length)
    }

    @tailrec
    private def doTailRec(items: List[Int], acc: Int, index: Int): Int = {
        items match {
            case head :: tail =>
                if (head % 2 == 0) doTailRec(tail, head * acc + index, index - 1)
                else doTailRec(tail, -1 * head * acc + index, index -1)
            case Nil => acc
        }
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        search(items, 0, items.length - 1, value)
    }

    @tailrec
    private def search(items: List[Int], low: Int, high: Int, value: Int): Option[Int] = {
        if (low > high) None
        else {
            val mid = low + (high - low) / 2
            items.lift(mid) match {
                case Some(midValue) =>
                    if (midValue == value) Some(mid)
                    else if (midValue > value) search(items, low, mid - 1, value)
                    else search(items, mid + 1, high, value)
                case None => None
            }
        }
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
        val letters = ('a' to 'z').toList
        List.fill(namesCount)(Random.shuffle(letters).take(3 + Random.nextInt(8)).mkString.capitalize)
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
        def findPhoneNumberSafe(num: String): Option[String] = Option(unsafePhoneService.findPhoneNumber(num))

        def addPhoneToBaseSafe(phone: String): Either[String, Unit] =
            Try(unsafePhoneService.addPhoneToBase(phone)).toEither.left.map(_.getMessage)

        def deletePhone(phone: String): Either[String, Unit] =
            Try(unsafePhoneService.deletePhone(phone)).toEither.left.map(_.getMessage)
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            val old = phoneServiceSafety.findPhoneNumberSafe(oldPhone)
            old match {
                case Some(phone) =>
                    val oldDeleted = phoneServiceSafety.deletePhone(phone)
                    oldDeleted match {
                        case Left(error) => error
                        case Right(_) =>
                            savePhone(newPhone)
                    }
                case None => savePhone(newPhone)
            }
        }

        private def savePhone(newPhone: String) = {
            val saved = phoneServiceSafety.addPhoneToBaseSafe(newPhone)
            saved match {
                case Left(error) => error
                case Right(_) => "ok"
            }
        }
    }
}
