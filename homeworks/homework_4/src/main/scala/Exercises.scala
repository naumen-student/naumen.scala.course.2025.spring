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
        items
          .indices
          .flatMap(i => items.indices.filter(j => items(i) + items(j) == sumValue && i != j)
            .lastOption
            .map(j => (i, j)))
          .lastOption.getOrElse((-1, -1))
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
        def calcSum(items: List[Int], acc: Int, index: Int) : Int = items match {
            case Nil => acc
            case head :: tail =>
                val newAcc = getNewAcc(head, acc, index)
                calcSum(tail, newAcc, index - 1)
        }

        calcSum(items.reverse, 1, items.size)
    }

    private def getNewAcc(head: Int, oldAcc: Int, index: Int): Int = {
        if (head % 2 == 0) head * oldAcc + index else -head * oldAcc + index
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        @tailrec
        def searcher(left: Int, right: Int): Option[Int] = {
            if (left > right) None
            else {
                val middle = (left + right) / 2
                items(middle) match {
                    case i if i == value => Some(middle)
                    case i if i > value => searcher(left, middle - 1)
                    case i if i < value => searcher(middle + 1, right)
                }
            }
        }

        searcher(0, items.size - 1)
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

        def generator : String = {
            val length = 6
            (1 to length).map { i =>
              val char = ('a' + Random.nextInt(26)).toChar
              if (i == 1) char.toUpper else char
            }.mkString
        }

        (1 to namesCount).map(_ => generator).toList
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
        def findPhoneNumberSafe(num: String) : Either[String, String] = {
            val result = unsafePhoneService.findPhoneNumber(num)
            if (result != null) Right(result) else Left(s"Phone with number $num was not found.")
        }

        def addPhoneToBaseSafe(phone: String): Either[String, String] = {
            try {
                unsafePhoneService.addPhoneToBase(phone)
                Right(s"Success add phone $phone.")
            } catch {
                case e: Exception => Left(s"Error when try to add phone $phone. Error message: ${e.getMessage}.")
            }
        }

        def deletePhone(phone: String): Either[String, String] = {
            try {
                unsafePhoneService.deletePhone(phone)
                Right(s"Success delete phone $phone.")
            } catch {
                case e: Exception => Left(s"Error when try to delete phone $phone. Error message: ${e.getMessage}")
            }
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
                case Right(old) =>
                    phoneServiceSafety.deletePhone(old) match {
                        case Right(_) =>
                            phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                                case Right(_) => "ok"
                                case Left(err) => err
                            }
                        case Left(err) => err
                    }
                case Left(err) => err
            }
        }
    }
}
