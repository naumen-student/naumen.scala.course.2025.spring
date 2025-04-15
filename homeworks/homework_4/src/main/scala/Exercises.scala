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
        val result = for {
            i <- items.indices
            j <- items.indices
            if items(i) + items(j) == sumValue && i != j
        } yield (i, j)

        if (result.isEmpty) (-1, -1) else result.head
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
        def loop(remaining: List[Int], index: Int, acc: Int): Int = {
            remaining match {
                case head :: tail =>
                    val newAcc = if (head % 2 == 0) {
                        head * acc + index
                    } else {
                        -1 * head * acc + index
                    }
                    loop(tail, index + 1, newAcc)
                case Nil => acc
            }
        }

        loop(items, 1, 1)
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        def search(low: Int, high: Int): Option[Int] = {
            if (low > high) None
            else {
                val mid = low + (high - low) / 2
                items(mid) match {
                    case v if v == value => Some(mid)
                    case v if v > value => search(low, mid - 1)
                    case _ => search(mid + 1, high)
                }
            }
        }

        if (items.isEmpty) None
        else search(0, items.length - 1)
    }

    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

    def generateNames(namesСount: Int): List[String] = {
        if (namesСount < 0) throw new Throwable("Invalid namesCount")

        val alphabet = ('A' to 'Z') ++ ('А' to 'Я')
        val lowerAlphabet = ('a' to 'z') ++ ('а' to 'я')

        def generateName(usedNames: Set[String]): (String, Set[String]) = {
            val firstChar = alphabet(scala.util.Random.nextInt(alphabet.length))
            val nameLength = scala.util.Random.nextInt(5) + 3 // Names between 3-7 chars

            val restOfName = (1 to nameLength).map(_ =>
                lowerAlphabet(scala.util.Random.nextInt(lowerAlphabet.length))).mkString

            val name = firstChar + restOfName

            if (usedNames.contains(name)) generateName(usedNames)
            else (name, usedNames + name)
        }

        def generateList(count: Int, acc: List[String], usedNames: Set[String]): List[String] = {
            if (count <= 0) acc
            else {
                val (name, newUsedNames) = generateName(usedNames)
                generateList(count - 1, name :: acc, newUsedNames)
            }
        }

        generateList(namesСount, Nil, Set.empty)
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
                Right(unsafePhoneService.addPhoneToBase(phone))
            } catch {
                case e: InternalError => Left(e.getMessage)
                case e: Throwable => Left(s"Unexpected error: ${e.getMessage}")
            }
        }

        def deletePhone(phone: String): Unit = {
            unsafePhoneService.deletePhone(phone)
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
                case Some(phone) => phoneServiceSafety.deletePhone(phone)
                case None =>
            }

            phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                case Right(_) => "ok"
                case Left(error) => error
            }
        }
    }
}
