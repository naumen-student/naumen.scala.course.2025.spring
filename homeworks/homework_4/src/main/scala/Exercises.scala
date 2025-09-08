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
            .flatMap(i => items.indices.map(j => (i, j)))
            .find { case (i, j) => i != j && items(i) + items(j) == sumValue }
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
        def helper(list: List[Int], acc: Int, index: Int): Int = {
            list match {
                case head :: tail =>
                    val newAcc = if (head % 2 == 0) head * acc + index else -1 * head * acc + index
                    helper(tail, newAcc, index + 1)
                case Nil => acc
            }
        }

        helper(items, 1, 1)
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
    @tailrec
    def binarySearch(low: Int, high: Int): Option[Int] = {
        if (low > high) None
        else {
            val mid = (low + high) / 2
            items(mid) match {
                case v if v == value => Some(mid)
                case v if v > value  => binarySearch(low, mid - 1)
                case _               => binarySearch(mid + 1, high)
            }
        }
    }

    binarySearch(0, items.length - 1)
}


    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

    def generateNames(namesCount: Int): List[String] = {
        val rand = new scala.util.Random

        def randomName(): String = {
            val length = rand.nextInt(5) + 3 // длина от 3 до 7
            val first = rand.nextPrintableChar().toUpper
            val rest = (1 until length)
                .map(_ => (rand.nextInt(26) + 'a').toChar)
                .mkString
            s"$first$rest"
        }

        List.fill(namesCount)(randomName()).filter(_.forall(_.isLetter))
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

    def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
        try {
            unsafePhoneService.addPhoneToBase(phone)
            Right(())
        } catch {
            case e: Exception => Left(e.getMessage)
        }
    }

    def deletePhone(phone: String): Either[String, Unit] = {
        try {
            unsafePhoneService.deletePhone(phone)
            Right(())
        } catch {
            case e: Exception => Left(e.getMessage)
        }
    }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
                case Some(phoneRecord) =>
                    phoneServiceSafety.deletePhone(phoneRecord) match {
                        case Right(_) =>
                            phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                                case Right(_) => "ok"
                                case Left(err) => s"Ошибка при добавлении нового номера: $err"
                            }
                        case Left(err) => s"Ошибка при удалении номера: $err"
                    }
                case None =>
                    phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                        case Right(_) => "ok"
                        case Left(err) => s"Ошибка при добавлении нового номера: $err"
                    }
            }
        }
    }

}
