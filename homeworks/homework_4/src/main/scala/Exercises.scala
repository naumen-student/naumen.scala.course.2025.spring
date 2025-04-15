import scala.annotation.tailrec
import Utils._
import scala.util.{Try, Random, Success, Failure}

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
        val pairs = for {
            i <- items.indices
            j <- items.indices
            if i != j && items(i) + items(j) == sumValue
        } yield (i, j)

        pairs.lastOption.getOrElse((-1, -1))
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
        def helper(reversed: List[Int], index: Int, acc: Int): Int = {
            reversed match {
                case head :: tail =>
                    val value = if (head % 2 == 0) head else -1 * head
                    val updated = value * acc + index
                    helper(tail, index - 1, updated)
                case Nil => acc
            }
        }

        helper(items.reverse, items.length, 1)
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
                val mid = low + (high - low) / 2
                val midVal = items(mid)

                if (midVal == value) Some(mid)
                else if (midVal < value) helper(mid + 1, high)
                else helper(low, mid - 1)
            }
        }

        helper(0, items.length - 1)
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

        def randomLetter(isUpper: Boolean): Char = {
            val base = if (isUpper) 'A' else 'a'
            (base + Random.nextInt(26)).toChar
        }

        def randomName(): String = {
            val length = 3 + Random.nextInt(6) // имя длиной от 3 до 8
            val firstLetter = randomLetter(isUpper = true)
            val rest = List.fill(length - 1)(randomLetter(isUpper = false)).mkString
            firstLetter + rest
        }

        List.fill(namesCount)(randomName())
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
    class PhoneServiceSafety(unsafe: SimplePhoneService) {

        def findPhoneNumberSafe(num: String): Option[String] =
            Option(unsafe.findPhoneNumber(num))

        def addPhoneToBaseSafe(phone: String): Either[String, Unit] =
            Try(unsafe.addPhoneToBase(phone)).toEither.left.map(_.getMessage)

        def deletePhoneSafe(phone: String): Either[String, Unit] =
            Try(unsafe.deletePhone(phone)).toEither.left.map(_.getMessage)
    }

    class ChangePhoneServiceSafe(phoneService: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            phoneService.findPhoneNumberSafe(oldPhone) match {
                case Some(oldRecord) =>
                    phoneService.deletePhoneSafe(oldRecord) match {
                        case Right(_) =>
                            phoneService.addPhoneToBaseSafe(newPhone) match {
                                case Right(_) => "ok"
                                case Left(error) => s"Error adding new phone: $error"
                            }
                        case Left(error) => s"Error deleting old phone: $error"
                    }

                case None =>
                    phoneService.addPhoneToBaseSafe(newPhone) match {
                        case Right(_) => "ok"
                        case Left(error) => s"Error adding new phone: $error"
                    }
            }
        }
    }
}
