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
    implicit val catShow: Show[Cat] = new Show[Cat] {
      def show(a: Cat): String = a match {
        case VeryLittleCat(name) => s"очень маленький кот $name"
        case LittleCat(name) => s"маленький кот $name"
        case NormalCat(name) => s"кот $name"
        case BigCat(name) => s"большой кот $name"
        case VeryBigCat(name) => s"очень большой кот $name"
      }
    }

    implicit def boxShow[A: Show]: Show[Box[A]] = new Show[Box[A]] {
      def show(box: Box[A]): String = box match {
        case BoxWith(value) => s"${value.show} в коробке"
        case EmptyBox => "пустая коробка"
      }
    }
  }

  object ShowSyntax {
    implicit class ShowOps[A](val a: A) {
      def show(implicit show: Show[A]): String = show.show(a)
    }
  }
}