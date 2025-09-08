
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
        var result = (-1, -1)
        
        for {
            i <- items.indices
            j <- items.indices
            if i != j && items(i) + items(j) == sumValue
        } {
            result = (i, j)
        }
        
        result
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
        import scala.annotation.tailrec
        
        // Нам нужно точно воспроизвести поведение исходной рекурсивной функции
        
        @tailrec
        def compute(remaining: List[Int], stack: List[Int], indexes: List[Int]): Int = {
            remaining match {
                case Nil =>
                    // Когда список закончился, начинаем раскручивать стек
                    // и вычислять результат в том же порядке, что и в исходной функции
                    var result = 1
                    var i = 0
                    while (i < stack.length) {
                        val value = stack(i)
                        val index = indexes(i)
                        if (value % 2 == 0) {
                            result = value * result + index
                        } else {
                            result = -1 * value * result + index
                        }
                        i += 1
                    }
                    result
                
                case head :: tail =>
                    // Добавляем элемент в стек и продолжаем
                    compute(tail, head :: stack, (indexes.headOption.getOrElse(0) + 1) :: indexes)
            }
        }
        
        compute(items, Nil, Nil)
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        import scala.annotation.tailrec
        
        @tailrec
        def search(left: Int, right: Int): Option[Int] = {
            if (left > right) None
            else {
                val mid = left + (right - left) / 2
                
                if (items(mid) == value) Some(mid)
                else if (items(mid) > value) search(left, mid - 1)
                else search(mid + 1, right)
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

    def generateNames(namesCount: Int): List[String] = {
        if (namesCount < 0) throw new Throwable("Invalid namesCount")
        
        val consonants = "bcdfghjklmnpqrstvwxz".toList
        val vowels = "aeiouy".toList
        
        def generateName: String = {
            val nameLength = 5 + scala.util.Random.nextInt(6) // имена от 5 до 10 символов
            
            val firstLetter = consonants(scala.util.Random.nextInt(consonants.length)).toUpper
            
            val restLetters = (1 until nameLength).map { i =>
                if (i % 2 == 1) {
                    vowels(scala.util.Random.nextInt(vowels.length))
                } else {
                    consonants(scala.util.Random.nextInt(consonants.length))
                }
            }
            
            (firstLetter :: restLetters.toList).mkString
        }
        
        (0 until namesCount).map(_ => generateName).toList
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

        def deletePhone(phone: String): Option[Unit] = {
            findPhoneNumberSafe(phone).map(p => unsafePhoneService.deletePhone(p))
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            phoneServiceSafety.deletePhone(oldPhone)
            
            phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                case Right(_) => "ok"
                case Left(error) => error
            }
        }
    }
}
