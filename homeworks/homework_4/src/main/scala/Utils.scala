import scala.collection.mutable

object Utils {
    class PhoneBase(private val phones: mutable.ListBuffer[String]) {
        def insert(phone: String): Unit = phones += phone
        def list(): List[String] = phones.toList
        def delete(phone: String): Unit = phones.filter(_ != phone)

    }

    def checkPhoneNumber(num: String): Boolean = num.matches("^[\\+]?[(]?[0-9]{3}[)]?[-\\s\\.]?[0-9]{3}[-\\s\\.]?[0-9]{4,6}$")

    class SimplePhoneService(phonesBase: PhoneBase) {

        def findPhoneNumber(num: String): String = {
            val resulNums = phonesBase.list().filter(_ == num)
            if (resulNums.isEmpty)
                null
            else
                resulNums.head
        }

        def addPhoneToBase(phone: String): Unit = {
            if (checkPhoneNumber(phone))
                phonesBase.insert(phone)
            else
                throw new InternalError("Invalid phone string")
        }

        def deletePhone(phone: String): Unit = phonesBase.delete(phone)
    }

    trait ChangePhoneService {
        def changePhone(oldPhone: String, newPhone: String): String
    }
}
