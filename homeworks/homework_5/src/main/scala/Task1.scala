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
        case LittleCat(name)     => s"маленький кот $name"
        case NormalCat(name)    => s"кот $name"
        case BigCat(name)       => s"большой кот $name"
        case VeryBigCat(name)   => s"очень большой кот $name"
      }
    }

    implicit def boxShow[A](implicit showA: Show[A]): Show[Box[A]] = new Show[Box[A]] {
      def show(box: Box[A]): String = box match {
        case BoxWith(value) => s"${showA.show(value)} в коробке"
        case EmptyBox       => "пустая коробка"
      }
    }
  }

  object ShowSyntax {
    implicit class ShowOps[A](val a: A) extends AnyVal {
      def show(implicit show: Show[A]): String = show.show(a)
    }
  }
}