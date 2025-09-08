/*
  Задание №1
  В задание уже описан тайп класс и синтакс для него.
  Вам необходимо в объекте ShowInstance описать инстансы тайп класса
  для типа Cat и Box.
  Тип Cat, в соответствии с тем, какого конкретно наследника этого типа мы хотим показать,
  должен отображаться следующим образом:
  VeryLittleCat - очень маленький кот его_имя
  LittleCat - маленький кот его_имя
  NormalCat - кот его_имя
  BigCat - большой кот его_имя
  VeryBigCat - очень большой кот его_имя

  Если кот будет в коробке, то к тому, что должно выводиться для кота
  необходимо добавить "в коробке". Если коробка пустая, то выводить "пустая коробка"

  В тестах можно всегда более точно посмотреть фразы.
 */
object Task1 extends App {
  trait Show[-A] {
    def show(a: A): String
  }

  sealed trait Cat {
    def name: String
  }
  case class VeryLittleCat(name: String) extends Cat
  case class LittleCat(name: String) extends Cat
  case class NormalCat(name: String) extends Cat
  case class BigCat(name: String) extends Cat
  case class VeryBigCat(name: String) extends Cat

  sealed trait Box[+A] {
    def value: A
  }
  case class BoxWith[+A](value: A) extends Box[A]
  case object EmptyBox extends Box[Nothing] {
    override def value: Nothing = throw new Exception("Empty box!")
  }

  object ShowInstance {
    implicit val catShow: Show[Cat] = {
      case veryLittleCat: VeryLittleCat => s"очень маленький кот ${veryLittleCat.name}"
      case littleCat: LittleCat => s"маленький кот ${littleCat.name}"
      case normalCat: NormalCat => s"кот ${normalCat.name}"
      case bigCat: BigCat => s"большой кот ${bigCat.name}"
      case veryBigCat: VeryBigCat => s"очень большой кот ${veryBigCat.name}"
    }

    implicit def boxShow[A](implicit showA: Show[A]): Show[Box[A]] = {
      case BoxWith(value) => s"${showA.show(value)} в коробке"
      case EmptyBox => "пустая коробка"
    }
  }

  object ShowSyntax {
    implicit class ShowOps[A](val a: A) {
      def show(implicit show: Show[A]): String = show.show(a)
    }
  }

  import ShowInstance._
  import ShowSyntax._

  println(VeryLittleCat("Мурзик").show) // очень маленький кот Мурзик
  println(LittleCat("Мурзик").show) // маленький кот Мурзик
  println(NormalCat("Мурзик").show) // кот Мурзик
  println(BigCat("Мурзик").show) // большой кот Мурзик
  println(VeryBigCat("Мурзик").show) // очень большой кот Мурзик

  println(BoxWith(VeryLittleCat("Мурзик")).show) // очень маленький кот Мурзик в коробке
  println(EmptyBox.show) // пустая коробка
}