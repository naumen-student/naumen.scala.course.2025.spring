import scala.annotation.tailrec
import scala.language.postfixOps
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
        items.indices.flatMap(i =>
              items.indices.filter(j => items(i) + items(j) == sumValue && i != j)
                .lastOption.map(j => (i, j))).lastOption.getOrElse((-1, -1))
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

    def tailRecRecursion(items: List[Int]): Int =
        items.zipWithIndex.foldRight(1) {
              case ((el, index), acc) =>
                  (if (el % 2 == 0) el else -el) * acc + index + 1
          }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        @tailrec
        def binarySearchRecursive(left: Int, right: Int): Option[Int] = {
            if (left > right) {
                None
            } else {
                val mid = (left + right) / 2
                if (items(mid) == value)
                    Some(mid)
                else if (items(mid) > value)
                    binarySearchRecursive(left, mid - 1)
                else
                    binarySearchRecursive(mid + 1, right)
            }
        }

        binarySearchRecursive(0, items.length - 1)
    }

    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

    def generateNames(namesCount: Int): List[String] = {
        val alphabet = 'a' to 'z'
        val maxLength = 100

        def randomName: String = {
            val firstLetter = alphabet(Random.nextInt(alphabet.size)).toUpper
            val otherLetters = (1 to Random.nextInt(maxLength - 1))
              .map(_ => alphabet(Random.nextInt(alphabet.size))).mkString
            firstLetter + otherLetters
        }

        List.fill(namesCount)(randomName)
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

    class PhoneServiceSafety(unsafePhoneService: SimplePhoneService) {

        def findPhoneNumberSafe(num: String): Option[String] =
            Option(unsafePhoneService.findPhoneNumber(num))

        def addPhoneToBaseSafe(phone: String): Either[String, Unit] =
            if (!checkPhoneNumber(phone)) Left("Invalid phone number format")
            else {
                unsafePhoneService.addPhoneToBase(phone)
                Right(())
            }

        def deletePhoneSafe(phone: String): Either[String, Unit] =
            findPhoneNumberSafe(phone) match {
                case Some(_) =>
                    unsafePhoneService.deletePhone(phone)
                    Right(())
                case None => Left("Old phone number not found")
            }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            for {
                _ <- phoneServiceSafety.deletePhoneSafe(oldPhone)
                _ <- phoneServiceSafety.addPhoneToBaseSafe(newPhone)
            } yield "ok"
        }.fold(error => s"Error: $error", identity)
    }
}
