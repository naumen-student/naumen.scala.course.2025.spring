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
        (for {
            (value, i) <- items.zipWithIndex
            (otherValue, j) <- items.zipWithIndex
            if i != j && value + otherValue == sumValue
        } yield (i, j)).lastOption.getOrElse((-1, -1))
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
        def loop(remaining: List[Int], index: Int, continuation: Int => Int): Int = remaining match {
            case Nil => continuation(1)
            case head :: tail =>
                val multiplier = if (head % 2 == 0) head else -head
                loop(tail, index + 1, result => continuation(multiplier * result + index))
        }
        loop(items, 1, identity)
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        @tailrec
        def search(left: Int, right: Int): Option[Int] = {
            if (left > right) {
                None
            } else {
                val mid = left + (right - left) / 2
                items(mid) match {
                    case x if x == value => Some(mid)
                    case x if x < value => search(mid + 1, right)
                    case _ => search(left, mid - 1)
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

    def generateNames(namesСount: Int): List[String] = {
        if (namesСount < 0) throw new Throwable("Invalid namesCount")
        
        def generateName: String = {
            val nameLength = Random.nextInt(7) + 3 // длина от 3 до 9
            val firstChar = ('A' + Random.nextInt(26)).toChar
            val restChars = (1 until nameLength).map(_ => ('a' + Random.nextInt(26)).toChar)
            (firstChar +: restChars).mkString
        }
        
        @tailrec
        def generateUniqueNames(count: Int, acc: Set[String]): List[String] = {
            if (acc.size >= count) acc.take(count).toList
            else {
                val newName = generateName
                generateUniqueNames(count, acc + newName)
            }
        }
        
        if (namesСount == 0) Nil else generateUniqueNames(namesСount, Set.empty)
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
                case ex: Exception => Left(s"Failed to add phone: ${ex.getMessage}")
            }

        def deletePhone(phone: String): Either[String, Unit] = 
            try {
                unsafePhoneService.deletePhone(phone)
                Right(())
            } catch {
                case ex: Exception => Left(s"Failed to delete phone: ${ex.getMessage}")
            }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            val result = for {
                _ <- phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
                    case Some(_) => phoneServiceSafety.deletePhone(oldPhone)
                    case None => Right(())
                }
                _ <- phoneServiceSafety.addPhoneToBaseSafe(newPhone)
            } yield "ok"
            
            result.fold(identity, identity)
        }
    }
}