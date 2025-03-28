
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
        items.zipWithIndex
          .combinations(2)
          .find(l => l(0)._1 + l(1)._1 == sumValue)
          .map(l => (l(0)._2, l(1)._2).swap)
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
        def loop(items: List[Int], index: Int, result: Int): Int = {
            items match {
                case head :: tail =>
                    if (head % 2 == 0) {
                        loop(tail, index - 1, head * result +index)
                    } else {
                        loop(tail, index - 1, index - head * result)
                    }
                case _ => result
            }
        }
        loop(items.reverse, items.length, 1)
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        @tailrec
        def search(low: Int, high: Int): Option[Int] = {
            if (low > high) {
                None
            } else {
                val mid = (low + high) / 2
                items.lift(mid) match {
                    case Some(midValue) =>
                        if (midValue == value) {
                            Some(mid)
                        } else if (midValue > value) {
                            search(low, mid - 1)
                        } else {
                            search(mid + 1, high)
                        }
                    case None => None
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

    def generateNames(namesСount: Int): List[String] = {
        require(namesСount >= 0, "Invalid namesCount")
        List.tabulate(namesСount)(_ => Random.alphanumeric.filter(_.isLetter).take(Random.nextInt(10) + 2).mkString.toLowerCase.capitalize)
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

        def addPhoneToBaseSafe(phone: String): Either[String, String] = {
            try {
                unsafePhoneService.addPhoneToBase(phone)
                Right("Phone added successfully")
            } catch {
                case e: Exception => Left(s"Failed to add phone: ${e.getMessage}")
            }

        def deletePhone(phone: String): Either[String, String] = {
            try {
                unsafePhoneService.deletePhone(phone)
                Right("Phone deleted successfully")
            } catch {
                case e: Exception => Left(s"Failed to delete phone: ${e.getMessage}")
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
                case None =>
                    phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                        case Right(_) => "ok"
                        case Left(error) => s"Failed to add new phone: $error"
                    }
            }
        }
    }
}
