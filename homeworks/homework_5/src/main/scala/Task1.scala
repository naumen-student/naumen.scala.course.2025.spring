object ShowInstance {
  import Task1._

  implicit val catShow: Show[Cat] = new Show[Cat] {
    def show(a: Cat): String = a match {
      case VeryLittleCat(name) => s"очень маленький кот $name"
      case LittleCat(name)     => s"маленький кот $name"
      case NormalCat(name)     => s"кот $name"
      case BigCat(name)        => s"большой кот $name"
      case VeryBigCat(name)    => s"очень большой кот $name"
    }
  }

  implicit def boxShow[A](implicit showA: Show[A]): Show[Box[A]] = new Show[Box[A]] {
    def show(box: Box[A]): String = box match {
      case EmptyBox        => "пустая коробка"
      case BoxWith(value)  => s"${showA.show(value)} в коробке"
    }
  }
}
