import scala.annotation.tailrec
import scala.util.Random

object Exercises {
  // Задание 1: Поиск двух чисел
  def findSumImperative(items: List[Int], sumValue: Int): (Int, Int) = {
    for (i <- items.indices) {
      for (j <- i + 1 until items.length) {
        if (items(i) + items(j) == sumValue) return (i, j)
      }
    }
    (-1, -1)
  }

  def findSumFunctional(items: List[Int], sumValue: Int): (Int, Int) = {
    items.zipWithIndex
      .flatMap { case (x, i) =>
        items.zipWithIndex.find { case (y, j) => i < j && x + y == sumValue }
          .map { case (_, j) => (i, j) }
      }
      .headOption.getOrElse((-1, -1))
  }

  // Задание 2: Рекурсия
  def simpleRecursion(items: List[Int], index: Int = 1): Int = items match {
    case Nil => 1
    case head :: tail =>
      val res = simpleRecursion(tail, index + 1)
      if (head % 2 == 0) head * res + index else -1 * head * res + index
  }

  def tailRecRecursion(items: List[Int]): Int = {
    @tailrec
    def helper(rem: List[Int], idx: Int, acc: Int): Int = rem match {
      case Nil => acc
      case h :: t =>
        val newAcc = if (h % 2 == 0) h * acc + idx else -1 * h * acc + idx
        helper(t, idx - 1, newAcc)
    }
    helper(items.reverse, items.length, 1)
  }

  // Задание 3: Бинарный поиск
  def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
    @tailrec
    def search(low: Int, high: Int): Option[Int] = {
      if (low > high) None else {
        val mid = low + (high - low) / 2
        items(mid) match {
          case m if m == value => Some(mid)
          case m if m < value  => search(mid + 1, high)
          case _               => search(low, mid - 1)
        }
      }
    }
    if (items.isEmpty) None else search(0, items.length - 1)
  }

  // Задание 4: Генерация имен
  def generateNames(namesCount: Int): List[String] = {
    if (namesCount < 0) throw new IllegalArgumentException("Invalid namesCount")
    val rnd = new Random
    def randomName: String = {
      val length = rnd.nextInt(8) + 3
      val firstChar = (rnd.nextInt(26) + 65).toChar
      val rest = (1 until length).map(_ => (rnd.nextInt(26) + 97).toChar).mkString
      firstChar + rest
    }
    Stream.continually(randomName).distinct.take(namesCount).toList
  }
}

// Задание 5: Интерфейсы и реализация
trait SimplePhoneService {
  def findPhoneNumber(num: String): String
  def addPhoneToBase(phone: String): Unit
  def deletePhone(phone: String): Unit
}

trait ChangePhoneService {
  def changePhone(oldPhone: String, newPhone: String): String
}

object SideEffectExercise {
  class PhoneServiceSafety(unsafeService: SimplePhoneService) {
    def findPhoneNumberSafe(num: String): Option[String] =
      Option(unsafeService.findPhoneNumber(num))

    def addPhoneToBaseSafe(phone: String): Either[String, Unit] =
      try { unsafeService.addPhoneToBase(phone); Right(()) }
      catch { case e: Exception => Left(e.getMessage) }

    def deletePhone(phone: String): Either[String, Unit] =
      try { unsafeService.deletePhone(phone); Right(()) }
      catch { case e: Exception => Left(e.getMessage) }
  }

  class ChangePhoneServiceSafe(safeService: PhoneServiceSafety) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      (for {
        _ <- safeService.findPhoneNumberSafe(oldPhone).toRight("Old phone not found")
        _ <- safeService.deletePhone(oldPhone)
        _ <- safeService.addPhoneToBaseSafe(newPhone)
      } yield "ok").merge
    }
  }
}