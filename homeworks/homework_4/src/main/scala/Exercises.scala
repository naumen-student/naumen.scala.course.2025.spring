
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
        val indices = items.indices.toList
        val validPairs = for {
            i <- indices
            j <- indices
            if i != j && items(i) + items(j) == sumValue
        } yield (i, j)
        validPairs.lastOption.getOrElse((-1, -1))
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

    import scala.annotation.tailrec

    def tailRecRecursion(items: List[Int]): Int = {
        @tailrec
        def helper(items: List[Int], acc: Int, index: Int): Int = items match {
            case Nil => acc
            case head :: tail =>
                val newAcc = if (head % 2 == 0) head * acc else -head * acc
                helper(tail, newAcc + index, index - 1) // Ensure the '+' is inside the newAcc computation
        }
        val reversed = items.reverse
        val n = reversed.length
        helper(reversed, 1, n)
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        val array = items.toArray
        def helper(left: Int, right: Int): Option[Int] = {
            if (left > right) None
            else {
                val mid = left + (right - left) / 2
                val midValue = array(mid)
                if (midValue == value) Some(mid)
                else if (midValue < value) helper(mid + 1, right)
                else helper(left, mid - 1)
            }
        }
        helper(0, array.length - 1)
    }


    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

    import scala.util.Random

    def generateNames(namesCount: Int): List[String] = {
        require(namesCount >= 0, "namesCount must be non-negative")

        val random = new Random

        def generateName(): String = {
            val minLength = 3
            val maxLength = 10
            val length = random.nextInt(maxLength - minLength + 1) + minLength

            val firstChar = ('A' + random.nextInt(26)).toChar
            val otherChars = List.fill(length - 1) {
                ('a' + random.nextInt(26)).toChar
            }

            (firstChar +: otherChars).mkString
        }

        List.fill(namesCount)(generateName())
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

import scala.util.Try

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
        // Returns Option[String] for safe lookup
        def findPhoneNumberSafe(num: String): Option[String] =
            Option(unsafePhoneService.findPhoneNumber(num))

        // Returns Either[String, Unit] for safe addition
        def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
            try {
                unsafePhoneService.addPhoneToBase(phone)
                Right(())
            } catch {
                case e: Exception => Left(s"Failed to add phone: ${e.getMessage}")
            }
        }

        // Returns Either[String, Unit] for safe deletion
        def deletePhoneSafe(phone: String): Either[String, Unit] = {
            try {
                unsafePhoneService.deletePhone(phone)
                Right(())
            } catch {
                case e: Exception => Left(s"Failed to delete phone: ${e.getMessage}")
            }
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            // Step 1: Check old phone existence
            phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
                case None => "Old phone not found"
                case Some(oldPhoneFound) =>
                    // Step 2: Add new phone first (critical for data safety)
                    phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                        case Left(err) => err  // Return error if add fails
                        case Right(_) =>
                            // Step 3: Delete old phone after successful addition
                            phoneServiceSafety.deletePhoneSafe(oldPhoneFound) match {
                                case Left(err) => err  // Return error if delete fails
                                case Right(_) => "ok"  // Success
                            }
                    }
            }
        }
    }
}


