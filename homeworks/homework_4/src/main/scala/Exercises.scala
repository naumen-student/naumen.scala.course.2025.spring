
object Exercises {
  def findSumImperative(list: List[Int], target: Int): Boolean = {
    val seen = scala.collection.mutable.HashSet[Int]()
    for (n <- list) {
      if (seen.contains(target - n)) return true
      seen.add(n)
    }
    false
  }

  def findSumFunctional(list: List[Int], target: Int): Boolean = {
    list.foldLeft((Set.empty[Int], false)) {
      case ((seen, true), _) => (seen, true)
      case ((seen, false), n) =>
        if (seen.contains(target - n)) (seen, true)
        else (seen + n, false)
    }._2
  }


  def simpleRecursion(list: List[Int]): Int = list match {
    case Nil => 0
    case head :: tail => head + simpleRecursion(tail)
  }

  def tailRecRecursion(list: List[Int]): Int = {
    @annotation.tailrec
    def internal(remaining: List[Int], acc: Int): Int = remaining match {
      case Nil => acc
      case head :: tail => internal(tail, acc + head)
    }

    internal(list, 0)
  }


  def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
    @annotation.tailrec
    def internal(left: Int, right: Int): Option[Int] = {
      if (left > right) None
      else {
        val mid = (left + right) / 2
        if (items(mid) == value) Some(mid)
        else if (items(mid) > value) internal(left, mid - 1)
        else internal(mid + 1, right)
      }
    }

    internal(0, items.length - 1)
  }

  def generateNames(namesCount: Int): List[String] = {
    if (namesCount < 0) throw new Throwable("Invalid namesCount")

    val rand = new scala.util.Random

    val upperLetters = ('A' to 'Z') ++ ('А' to 'Я')
    val lowerLetters = ('a' to 'z') ++ ('а' to 'я')

    def randomName: String = {
      val first = upperLetters(rand.nextInt(upperLetters.length))
      val length = rand.nextInt(10) + 1 // хотя бы одна буква после первой
      val rest = (1 to length).map(_ => lowerLetters(rand.nextInt(lowerLetters.length))).mkString
      first + rest
    }

    Stream.continually(randomName).distinct.take(namesCount).toList
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

  class PhoneServiceSafety(unsafePhoneService: SimplePhoneService) {
    def findPhoneNumberSafe(num: String): Option[String] = {
      Option(unsafePhoneService.findPhoneNumber(num))
    }

    def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
      try {
        unsafePhoneService.addPhoneToBase(phone)
        Right(())
      } catch {
        case _: InternalError => Left("Invalid phone string")
      }
    }

    def deletePhone(phone: String): Unit = {
      unsafePhoneService.deletePhone(phone)
    }
  }

  class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
        case Some(_) =>
          phoneServiceSafety.deletePhone(oldPhone)
          phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
            case Right(_) => "ok"
            case Left(err) => err
          }
        case None => "Old phone number not found"
      }
    }
  }
}
