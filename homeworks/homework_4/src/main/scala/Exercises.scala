import scala.annotation.tailrec

import scala.util.{Random,Try,Success,Failure}




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
        result
    }

    def findSumFunctional(items: List[Int], sumValue: Int) = {
        (-1, -1)
      }

    }

    result

  }




  def findSumFunctional(items: List[Int], sumValue: Int) = {

    items.zipWithIndex.flatMap(

      first => items.zipWithIndex.filter(second => first._1 + second._1 == sumValue && first._2 != second._2).map(second => (first._2, second._2))

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
        1
      case _ => 1

    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */
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

    def binarySearch(l: Int, r: Int): Option[Int] =

      Some((l + r) / 2).filter(_ => l <= r) match {

        case None => None

        case Some(m) =>

          if (items(m) == value) Some(m)

          else if (items(m) > value) binarySearch(l, m - 1)

          else binarySearch(m + 1, r)

      }




    binarySearch(0, items.length - 1)

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

    else List.tabulate(namesCount)(_ =>

      Random.alphanumeric.filter(_.isLetter).take(10).mkString.toLowerCase.capitalize

    )

  }


    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        None
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
        Nil
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



	@@ -96,29 +127,42 @@ object Exercises {

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

    def findPhoneNumberSafe(num: String): Option[String] = Option(unsafePhoneService.findPhoneNumber(num))


    class PhoneServiceSafety(unsafePhoneService: SimplePhoneService) {
        def findPhoneNumberSafe(num: String) = ???

        def addPhoneToBaseSafe(phone: String) = ???

        def deletePhone(phone: String) = ???
    def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {

      Try(unsafePhoneService.addPhoneToBase(phone)) match {

        case Success(ok) => Right(ok)

        case Failure(exception) => Left(exception.getMessage)

      }

    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = ???



    def deletePhone(phone: String): Option[Unit] = Option(findPhoneNumberSafe(phone).map(unsafePhoneService.deletePhone))

  }




  class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {




    override def changePhone(oldPhone: String, newPhone: String): String = {

      phoneServiceSafety.findPhoneNumberSafe(oldPhone).foreach(phoneServiceSafety.deletePhone)

      phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {

        case Right(_) => "ok"

        case Left(message) => message

      }

    }
}

  }

}
