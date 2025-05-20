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
        items.zipWithIndex.flatMap(
            i => items.zipWithIndex.filter(j => i._1 + j._1 == sumValue && i._2 != j._2).map(j => (i._2, j._2))
        ).lastOption.getOrElse((-1, -1))
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
        def rec(items: List[Int], index: Int, result: Int): Int =
            items match {
                case head :: tail =>
                    if (head % 2 == 0) {
                        rec(tail, index - 1, index + head * result)
                    } else {
                        rec(tail, index - 1, index - head * result)
                    }
                case _ => result
            }

        rec(items.reverse, items.length, 1)
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
                if (items(mid) == value) Some(mid)
                else if (items(mid) > value) search(low, mid - 1)
                else search(mid + 1, high)
            }
        }

        search(0, items.size - 1)
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

        val random = new scala.util.Random

        def generateName: String = {
            val nameLength = random.nextInt(5) + 3 // длина имени от 3 до 7 символов
            val name = (1 to nameLength).map { _ =>
                (random.nextInt(26) + 97).toChar
            }.mkString
            name.capitalize
        }

        List.fill(namesCount)(generateName)
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
                Option(unsafePhoneService.findPhoneNumber(num)).filter(_ != null)
            }

            def addPhoneToBaseSafe(phone: String): Option[Unit] = {
                if (checkPhoneNumber(phone)) {
                    unsafePhoneService.addPhoneToBase(phone)
                    Some(())
                } else {
                    None
                }
            }

            def deletePhoneSafe(phone: String): Option[Unit] = {
                Option(phone).filter(_ != null).map(_ => unsafePhoneService.deletePhone(phone))
            }
        }

        class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
            override def changePhone(oldPhone: String, newPhone: String): String = {
                (for {
                    _ <- phoneServiceSafety.findPhoneNumberSafe(oldPhone)
                    _ <- phoneServiceSafety.deletePhoneSafe(oldPhone)
                    _ <- phoneServiceSafety.addPhoneToBaseSafe(newPhone)
                } yield "ok").getOrElse("Error")
            }
        }
    }
}