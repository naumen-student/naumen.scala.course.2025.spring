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
        @tailrec
        def funcSum(index: Int): (Int, Int) = {
            if (index >= items.length) {
                (-1, -1)
            }
            else {
                val diffIndex = items.indexOf(sumValue - items(index))
                if (diffIndex != -1 && diffIndex != index) {
                    (diffIndex, index)
                }
                else {
                    funcSum(index + 1)
                }
            }
        }
        funcSum(0)
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
        def tailRec(items: List[Int], index: Int, accumulator: Int = 1): Int = {
            items match {
                case head :: tail =>
                    val result = if (head % 2 == 0) {
                        head
                    }
                    else {
                        -head
                    }
                    tailRec(tail, index - 1, result * accumulator + index)
                case _ => accumulator
            }
        }
        tailRec(items.reverse, items.size)
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        @tailrec
        def binarySearch(first: Int, last: Int): Option[Int] =
            if (first > last) {
                None
            }
            else {
                val middleNum = (first + last) / 2
                items(middleNum) match {
                    case item if item == value => Some(middleNum)
                    case item if item <= value => binarySearch(middleNum + 1, last)
                    case _ => binarySearch(first, middleNum - 1)
                }
            }
        binarySearch(0, items.size - 1)
    }

    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

    def generateNames(namesCount: Int): List[String] = {
        if (namesCount < 0) {
            throw new Throwable("Invalid namesCount")
        }
        val chars = {
            ('a' to 'z') ++ ('A' to 'Z')
        }
        @tailrec
        def generateNamesRec(remainingCount: Int, accumulator: List[String] = Nil): List[String] = {
            if (remainingCount == 0) {
                accumulator.map(_.toLowerCase.capitalize)
            }
            else {
                generateNamesRec(remainingCount - 1, List.fill(10)(chars(Random.nextInt(chars.length))).mkString :: accumulator)
            }
        }
        generateNamesRec(namesCount)
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
        def safeFindPhone(num: String): Option[String] =
            Option(unsafePhoneService.findPhoneNumber(num))

        def safeAddPhone(phone: String): Either[String, Unit] =
            if (checkPhoneNumber(phone)) {
                Right(unsafePhoneService.addPhoneToBase(phone))
            }
            else Left("Incorrect phone string")

        def deletePhone(phone: String): Either[String, Unit] =
            safeFindPhone(phone) match {
                case Some(number) => Right(unsafePhoneService.deletePhone(number))
                case None => Left(s"Phone number $phone is not found")
            }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            phoneServiceSafety.safeFindPhone(oldPhone).map(phoneServiceSafety.deletePhone)
            phoneServiceSafety.safeAddPhone(newPhone) match {
                case Left(error) => error
                case _   => "ok"
            }
        }
    }
}