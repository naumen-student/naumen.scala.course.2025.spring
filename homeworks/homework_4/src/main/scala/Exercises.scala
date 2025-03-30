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

    def findSumFunctional(items: List[Int], sumValue: Int) = {
        @tailrec
        def func(i: Int, j: Int): (Int, Int) = {
            if (i < 0)
                (-1, -1)
            else
                if (items(i) + items(j) == sumValue && i != j)
                    (i, j)
                else if (j > 0)
                    func(i, j - 1)
                else
                    func(i - 1, items.length - 1)
        }
        func(items.length - 1, items.length - 1)
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
        def tailRecRecursion(items: List[Int], acc: Int = 1): Int = {
            items match {
                case head :: tail =>
                    if (items.last % 2 == 0) {
                        tailRecRecursion(items.init, items.last * acc + items.length)
                    } else {
                        tailRecRecursion(items.init, -1 * items.last * acc + items.length)
                    }
                case _ => acc
            }
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
        def find(left: Int, right: Int): Option[Int] = {
            if (left > right) {
                None
            }
            else {
                val middle = (left + right) / 2
                if (value == items(middle)) {
                    Some(middle)
                } else {
                    if (value > items(middle))
                        find(middle + 1, right)
                    else
                        find(left, middle - 1)
                }
            }
        }
        find(0, items.length -1)
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
                if (namesСount == 0) {
            Nil
        } else {
            val firstLetters = ('А' until 'Я') ++ ('A' until 'Z')
            val allLetters = ('а' until 'я') ++ ('a' until 'z')
            (1 to namesСount).map(x => Random.shuffle(firstLetters).head.toString +
                (0 to 6).map(t => Random.shuffle(allLetters).head.toString).mkString)
              .toList
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
        def findPhoneNumberSafe(num: String): Option[String] = Some(unsafePhoneService.findPhoneNumber(num))

        def addPhoneToBaseSafe(phone: String): Either[InternalError, String] = {
            try {
                unsafePhoneService.addPhoneToBase(phone)
                Right("added")
            }
            catch {
                case error: InternalError => Left(error)
            }
        }

        def deletePhone(phone: String): Either[Error, String] = {
            try {
                unsafePhoneService.deletePhone(phone)
                Right("deleted")
            }
            catch {
                case error: Error => Left(error)
            }
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
                case Some(oldPhoneRecord) =>
                    phoneServiceSafety.deletePhone(oldPhoneRecord) match {
                        case Right(_) =>
                            tryAddPhone(newPhone)
                        case Left(error) => error.getMessage
                    }
                case None =>
                    tryAddPhone(newPhone)
            }
        }

        def tryAddPhone(newPhone: String): String = {
            phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                case Right(_) => "ok"
                case Left(error) => error.getMessage
            }
        }
    }
}
