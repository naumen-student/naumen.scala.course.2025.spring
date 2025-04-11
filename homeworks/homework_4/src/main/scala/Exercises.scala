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
        val indexedItems = items.zipWithIndex

        indexedItems.foldLeft((-1, -1)) {
            case (pair, (value1, index1)) => indexedItems.foldLeft(pair) {
                case (inPair, (value2, index2)) =>
                    if (index1 != index2 && value1 + value2 == sumValue) (index1, index2)
                    else inPair
            }
        }
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
        def helper(items: List[Int], index: Int, acc: Int): Int = {
            items match {
                case Nil => acc
                case head :: tail =>
                    val newVal = if (head % 2 == 0) head * acc + index else -head * acc + index
                    helper(tail, index - 1, newVal)
            }
        }
        helper(items.reverse, items.size, 1)
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        @tailrec
        def helper(low: Int, high: Int): Option[Int] = {
            if (low > high) None
            else {
                val mid = (low + high) / 2
                items(mid) match {
                    case m if m == value => Some(mid)
                    case m if m < value => helper(mid + 1, high)
                    case _ => helper(low, mid - 1)
                }
            }
        }

        if (items.isEmpty) None else helper(0, items.length - 1)
    }

    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

    def generateNames(namesСount: Int): List[String] = {
        if (namesСount < 0) throw new IllegalArgumentException("Invalid namesCount")

        def generateName: String = {
            val length = Random.nextInt(5) + 3
            val name = (1 to length).map { i =>
                if (i == 1) ('A' + Random.nextInt(26)).toChar
                else ('a' + Random.nextInt(26)).toChar
            }.mkString("")

            name
        }

        (1 to namesСount).map(_ => generateName).toList
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
            Option(unsafePhoneService.findPhoneNumber(num))
        }

        def addPhoneToBaseSafe(phone: String): Either[String, String] = {
            try {
                unsafePhoneService.addPhoneToBase(phone)
                Right("Phone added successfully")
            } catch {
                case e: Exception => Left(s"Error adding phone: ${e.getMessage}")
            }
        }

        def deletePhoneSafe(phone: String): Either[String, String] = {
            try {
                val phoneRecord = unsafePhoneService.findPhoneNumber(phone)
                if (phoneRecord != null) {
                    unsafePhoneService.deletePhone(phoneRecord)
                    Right("Phone deleted successfully")
                } else {
                    Left("Phone not found")
                }
            } catch {
                case e: Exception => Left(s"Error deleting phone: ${e.getMessage}")
            }
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
                case Some(oldPhoneRecord) =>
                    phoneServiceSafety.deletePhoneSafe(oldPhoneRecord) match {
                        case Right(_) =>
                            phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                                case Right(_) => "ok"
                                case Left(error) => s"Failed to add new phone: $error"
                            }
                        case Left(error) => s"Failed to delete old phone: $error"
                    }
                case None => s"Old phone number not found"
            }
        }
    }
}
