import scala.annotation.tailrec
import scala.util.Random

object Exercises {

  def findSumImperative(items: List[Int], sumValue: Int): (Int, Int) = {
    for (i <- 0 until items.length; j <- i + 1 until items.length) {
      if (items(i) + items(j) == sumValue)
        return (i, j)
    }
    (-1, -1)
  }

  def findSumFunctional(items: List[Int], sumValue: Int): (Int, Int) = {
    items.zipWithIndex
      .combinations(2)
      .find {
        case List((a, _), (b, _)) => a + b == sumValue
        case _                    => false
      } match {
      case Some(List((_, i), (_, j))) => (i, j)
      case _                          => (-1, -1)
    }
  }

  def simpleRecursion(items: List[Int], index: Int = 1): Int = {
    items match {
      case head :: tail =>
        if (head % 2 == 0) head * simpleRecursion(tail, index + 1) + index
        else -head * simpleRecursion(tail, index + 1) + index
      case _ => 1
    }
  }

  // Переписана с использованием свёртки справа для получения идентичного результата simpleRecursion.
  def tailRecRecursion(items: List[Int]): Int = {
    val indexed = items.zip(Stream.from(1)).toList  // индекс начинается с 1
    indexed.reverse.foldLeft(1) { case (acc, (head, idx)) =>
      if (head % 2 == 0) head * acc + idx else -head * acc + idx
    }
  }

  def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
    @tailrec
    def search(low: Int, high: Int): Option[Int] = {
      if (low > high) None
      else {
        val mid = low + (high - low) / 2
        val midVal = items(mid)
        if (midVal == value) Some(mid)
        else if (midVal < value) search(mid + 1, high)
        else search(low, mid - 1)
      }
    }
    if (items.isEmpty) None else search(0, items.length - 1)
  }

  def randomName: String = {
    val length = 3 + Random.nextInt(6) // длина от 3 до 8 символов
    val first = (Random.nextInt(26) + 'A'.toInt).toChar
    val rest = (1 until length).map(_ => (Random.nextInt(26) + 'a'.toInt).toChar).mkString
    first + rest
  }

  def generateNames(namesCount: Int): List[String] = {
    if (namesCount < 0) throw new Throwable("Invalid namesCount")
    List.fill(namesCount)(randomName)
  }
}

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

  // Сервис, работающий с телефонами в безопасном стиле с использованием Either и Option.
  class PhoneServiceSafety(unsafePhoneService: SimplePhoneService) {
    def findPhoneNumberSafe(num: String): Option[String] = {
      Option(unsafePhoneService.findPhoneNumber(num))
    }

    def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
      try {
        unsafePhoneService.addPhoneToBase(phone)
        Right(())
      } catch {
        case e: InternalError => Left(e.getMessage)
      }
    }

    def deletePhoneSafe(phone: String): Unit = {
      unsafePhoneService.deletePhone(phone)
    }
  }

  // Безопасный сервис смены номера, возвращающий "ok" или описание ошибки.
  class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
        case Some(_) => phoneServiceSafety.deletePhoneSafe(oldPhone)
        case None    =>
      }
      phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
        case Right(_) => "ok"
        case Left(err) => err
      }
    }
  }
}