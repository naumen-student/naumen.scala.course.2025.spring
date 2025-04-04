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
        items.zipWithIndex.foldLeft((-1, -1)) {
            case (acc, (a, i)) => items.zipWithIndex.foldLeft(acc) {
                case (innerAcc, (b, j)) =>
                  if (i != j && a + b == sumValue) (i, j)
                  else innerAcc
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

    def tailRecRecursion(items: List[Int], index: Int = 1): Int = {
        @tailrec
        def innerFunc(items: List[Int], index: Int, acc: Int): Int = {
            items match {
                case head :: tail =>
                    var newVal = 0
                    if (head % 2 == 0){
                        newVal = head * acc + index
                    } else {
                        newVal = -1 * head * acc + index
                    }
                    innerFunc(tail, index - 1, newVal)
                case _ => acc
            }
        }
        innerFunc(items.reverse, items.size, 1)
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
                val mid = (low + high) / 2
                items(mid) match {
                    case m if m == value => Some(mid)
                    case m if m < value => search(mid + 1, high)
                    case _ => search(low, mid - 1)
                }
            }
        }
        if (items.isEmpty) None else search(0, items.length - 1)
    }

    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

    def generateNames(namesСount: Int, rng: Random = Random): List[String] = {
        val highLetters = 'A' to 'Z'
        val lowLetters = 'a' to 'z'

        def makeName(len: Int): String = {
            def charAt(pos: Int): Char = pos match {
                case 1 => highLetters(rng.nextInt(highLetters.length))
                case _ => lowLetters(rng.nextInt(lowLetters.length))
            }
            (1 to len).map(charAt).mkString
        }

        if (namesСount < 1) Nil
        else {
            val nameLengths = Seq.fill(namesСount)(3 + rng.nextInt(8))
            nameLengths.map(makeName).toList
        }
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
            Option(unsafePhoneService.findPhoneNumber(num)).filter(_ != null)

        def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
            try {
                unsafePhoneService.addPhoneToBase(phone)
                Right(())
            } catch {
                case _: InternalError => Left("Failed to add phone")
            }
        }

        def deletePhone(phone: String): Either[String, Unit] = {
            try {
                unsafePhoneService.deletePhone(phone)
                Right(())
            } catch {
                case _: Exception => Left("Error deleting phone")
            }
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            val oldRecord = phoneServiceSafety.findPhoneNumberSafe(oldPhone)
            oldRecord match {
                case Some(phone) => {
                    val oldPhoneDeleted = phoneServiceSafety.deletePhone(phone)
                    oldPhoneDeleted match {
                        case Left(error: String) => error
                        case Right(_) => processNewPhoneRegistration(phoneServiceSafety, newPhone)
                    }
                }
                case None => processNewPhoneRegistration(phoneServiceSafety, newPhone)
            }
        }

        def processNewPhoneRegistration(phoneServiceSafety: PhoneServiceSafety, newPhone: String): String = {
            val newAddedPhone = phoneServiceSafety.addPhoneToBaseSafe(newPhone)
            newAddedPhone match {
                case Left(error: String) => error
                case Right(_) => "ok"
            }
        }
    }
}
