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
        val n = items.length
        val indices = for {
            i <- 0 until n
            j <- 0 until n
            if items(i) + items(j) == sumValue && i != j
        } yield (i, j)
        
        if (indices.isEmpty) (-1, -1) else indices.last
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
        
        def withStack: Int = {
            // Создаем структуру для имитации стека вызовов
            sealed trait Op
            case class Call(items: List[Int], index: Int) extends Op
            case class Val(v: Int) extends Op
            
            @tailrec
            def eval(stack: List[Op], env: Map[List[Int], Int] = Map.empty): Int = {
                stack match {
                    case Val(v) :: Nil => v
                    case Val(v) :: Call(h :: t, idx) :: rest =>
                        val result = if (h % 2 == 0) h * v + idx else -1 * h * v + idx
                        eval(Val(result) :: rest, env)
                    case Call(Nil, _) :: rest =>
                        eval(Val(1) :: rest, env)
                    case Call(xs@(h :: t), idx) :: rest =>
                        if (env.contains(t)) {
                            val tResult = env(t)
                            val result = if (h % 2 == 0) h * tResult + idx else -1 * h * tResult + idx
                            eval(Val(result) :: rest, env)
                        } else {
                            eval(Call(t, idx + 1) :: Call(xs, idx) :: rest, env)
                        }
                }
            }
            
            eval(List(Call(items, 1)))
        }
        
        withStack
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        if (items.isEmpty) None
        else {
            def search(start: Int, end: Int): Option[Int] = {
                if (start > end) None
                else {
                    val mid = start + (end - start) / 2
                    if (items(mid) == value) Some(mid)
                    else if (items(mid) > value) search(start, mid - 1)
                    else search(mid + 1, end)
                }
            }
            try {
                search(0, items.size - 1)
            } catch {
                case _: Exception => None
            }
        }
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
        
        val alphabet = ('a' to 'z').toList
        
        def generateName: String = {
            val length = scala.util.Random.nextInt(10) + 3 
            val firstChar = alphabet(scala.util.Random.nextInt(alphabet.length)).toUpper
            val restChars = (1 until length).map(_ => alphabet(scala.util.Random.nextInt(alphabet.length)))
            
            (firstChar :: restChars.toList).mkString
        }
        
        def uniqueNames(count: Int, acc: Set[String] = Set.empty): List[String] = {
            if (acc.size == count) acc.toList
            else uniqueNames(count, acc + generateName)
        }
        
        uniqueNames(namesСount)
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
                case e: Exception => Left(s"Unexpected error: ${e.getMessage}")
            }
        }

        def deletePhone(phone: String): Option[Unit] = {
            findPhoneNumberSafe(phone).map { _ =>
                unsafePhoneService.deletePhone(phone)
            }
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
                case Some(_) => phoneServiceSafety.deletePhone(oldPhone)
                case None => None
            }
            
            phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                case Right(_) => "ok"
                case Left(error) => error
            }
        }
    }
}
