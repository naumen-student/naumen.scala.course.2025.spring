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
        items.indices.flatMap { j =>
            items.indices.collectFirst {
                case i if items(i) + items(j) == sumValue && i != j => (i, j)
            }
        }
        .headOption
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

    @tailrec
    def tailRecRecursion(items: List[Int], index: Int = 1, acc: Int = 0, product: Int = 1): Int = {
        items match {
            case head :: tail =>
                if (head % 2 == 0) {
                    tailRecRecursion(tail, index + 1, acc + index * product, product * head)
                } else {
                    tailRecRecursion(tail, index + 1, acc + index * product, product * (-head))
                }
            case _ => acc + product
        }
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
            if (low > high) None
            else {
                val mid = low + (high - low) / 2
                items(mid) match {
                    case x if x == value => Some(mid)
                    case x if x < value => search(mid + 1, high)
                    case _ => search(low, mid - 1)
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

    def generateNames(namesCount: Int): List[String] = {

        def isValidName(name: String): Boolean =
            name.matches("^[A-Z][a-z]+$")

        def generateRandomName(): String = {
            val vowels = "aeiou"
            val consonants = "bcdfghjklmnpqrstvwxyz"

            val length = Random.nextInt(5) + 3
            val name = new StringBuilder

            (0 until length).foreach { i =>
                val charPool = if (i % 2 == 0) consonants else vowels
                val c = charPool(Random.nextInt(charPool.length))
                if (i == 0) name.append(c.toUpper) else name.append(c)
            }

            name.toString
        }

        @tailrec
        def generateValidNames(count: Int, acc: List[String] = Nil): List[String] = {
            if (count <= 0) acc
            else {
                val name = generateRandomName()
                if (isValidName(name))
                    generateValidNames(count - 1, name :: acc)
                else
                    generateValidNames(count, acc)
            }
        }

        if (namesCount < 0) throw new Throwable("Invalid namesCount")
        else generateValidNames(namesCount)
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
    import scala.util.{Try, Success, Failure}

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

        def findPhoneNumberSafe(num: String): Either[String, String] = {
            Option(unsafePhoneService.findPhoneNumber(num)) match {
                case Some(phone) => Right(phone)
                case None => Left(s"Phone number $num not found")
            }
        }

        def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
            Try(unsafePhoneService.addPhoneToBase(phone)) match {
                case Success(_) => Right(())
                case Failure(e) => Left(e.getMessage)
            }
        }

        def deletePhoneSafe(phone: String): Either[String, Unit] = {
            Try(unsafePhoneService.deletePhone(phone)) match {
                case Success(_) => Right(())
                case Failure(e) => Left(e.getMessage)
            }
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {

            val result = for {
                _ <- phoneServiceSafety.findPhoneNumberSafe(oldPhone)
                _ <- phoneServiceSafety.deletePhoneSafe(oldPhone)
                _ <- phoneServiceSafety.addPhoneToBaseSafe(newPhone)
            } yield "ok"

            result match {
                case Right(msg) => msg
                case Left(error) => error
            }
        }
    }
}