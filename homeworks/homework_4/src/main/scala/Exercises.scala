
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
    // Перебираем все пары индексов (i, j), где i != j, затем выбираем те, что удовлетворяют условию,
    // и возвращаем последний найденный результат (или (-1,-1), если пара не найдена).
    items.indices
        .flatMap(i => items.indices.filter(_ != i).map(j => (i, j)))
        .filter { case (i, j) => items(i) + items(j) == sumValue }
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
    import scala.annotation.tailrec

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
        def loop(i: Int, acc: Int): Int = {
        if (i < 0) acc
        else {
            val index = i + 1
            val head = items(i)
         val newAcc = (if (head % 2 == 0) head * acc else -head * acc) + index
            loop(i - 1, newAcc)
        }
        }
        loop(items.length - 1, 1)
    }


////////////////////////////////


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
                val midVal = items(mid)
                if (midVal == value) Some(mid)
                else if (midVal > value) search(low, mid - 1)
                else search(mid + 1, high)
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
        if (namesCount < 0) throw new Throwable("Invalid namesCount")

        val rand = new scala.util.Random

        def randomChar(from: Char, to: Char): Char =
            (from + rand.nextInt(to - from + 1)).toChar

        def randomName(): String = {
            val nameLength = 3 + rand.nextInt(7)
            val first = randomChar('A', 'Z')
            val rest = List.fill(nameLength - 1)(randomChar('a', 'z')).mkString
            s"$first$rest"
        }

        List.fill(namesCount)(randomName())
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
            case _: Throwable => Left("Invalid phone string")
        }

    def deletePhone(phone: String): Unit =
        unsafePhoneService.deletePhone(phone)
}

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
        phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
            case Some(old) =>
                phoneServiceSafety.deletePhone(old)
                phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                    case Right(_) => "ok"
                    case Left(err) => err
                }
            case None => "Old phone number not found"
        }
    }
}
}
