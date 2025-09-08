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
        items.zipWithIndex
          .flatMap { case (x, i) =>
              items.zipWithIndex
                .filter { case (y, j) => x + y == sumValue && i != j }
                .map { case (_, j) => (i, j) }
          }
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
        @annotation.tailrec
        def loop(reversed: List[Int], index: Int, acc: Int): Int = reversed match {
            case Nil => acc
            case head :: tail =>
                val newAcc =
                    if (head % 2 == 0) head * acc + index
                    else -1 * head * acc + index
                loop(tail, index - 1, newAcc)
        }

        val size = items.length
        loop(items.reverse, size, 1)
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

    def generateNames(namesСount: Int): List[String] = {
        val alphabet = ('a' to 'z') ++ ('а' to 'я')
        val capitalLetters = ('A' to 'Z') ++ ('А' to 'Я')

        def generateName(): String = {
            val length = 1 + Random.nextInt(10)
            val firstLetter = capitalLetters(Random.nextInt(capitalLetters.size))
            val rest = (1 until length).map(_ => alphabet(Random.nextInt(alphabet.size))).mkString
            s"$firstLetter$rest"
        }

        Stream
          .continually(generateName())
          .distinct
          .take(namesСount)
          .toList
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
        def findPhoneNumberSafe(num: String): Option[String] =
            Option(unsafePhoneService.findPhoneNumber(num))

        def addPhoneToBaseSafe(phone: String): Either[String, Unit] =
            try {
                unsafePhoneService.addPhoneToBase(phone)
                Right(())
            } catch {
                case e: Exception => Left(e.getMessage)
            }

        def deletePhone(phone: String): Either[String, Unit] =
            try {
                unsafePhoneService.deletePhone(phone)
                Right(())
            } catch {
                case e: Exception => Left(e.getMessage)
            }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            val result = for {
                maybeOld <- Right(phoneServiceSafety.findPhoneNumberSafe(oldPhone)): Either[String, Option[String]]
                _ <- maybeOld match {
                    case Some(phone) => phoneServiceSafety.deletePhone(phone)
                    case None => Right(())
                }
                _ <- phoneServiceSafety.addPhoneToBaseSafe(newPhone)
            } yield "ok"

            result.fold(identity, identity)
        }
    }
}
