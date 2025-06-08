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
  // Обратите внимание, что здесь type class параматрезирован контрвариантно
  // благодаря этому инстанс Show[Cat] мы можем применить, например, к BigCat.
  // Однако, это может быть неудобно, так как нам для каждого нового наследника придётся менять
  // реализацию Show[Cat]. Из-за этого практически все библиотеки, которые предоставляют свои
  // тайп классы для их использования пользователем, делают тайп классы инвариантными по параметру.
  // Можете написать две реализации (при -А и А) и сравнить их.
  trait Show[-A] {
    def show(a: A): String
  }
@@ -45,9 +21,18 @@ object Task1 extends App {
  }

  object ShowInstance {
    implicit val catShow: Show[Cat] = ???
    implicit val catShow: Show[Cat] = {
      case VeryLittleCat(name) => s"очень маленький кот $name"
      case LittleCat(name) => s"маленький кот $name"
      case NormalCat(name) => s"кот $name"
      case BigCat(name) => s"большой кот $name"
      case VeryBigCat(name) => s"очень большой кот $name"
    }

    implicit def boxShow[A: Show]: Show[Box[A]] = ???
    implicit def boxShow[A: Show]: Show[Box[A]] = {
      case EmptyBox => "пустая коробка"
      case BoxWith(a) => s"${implicitly[Show[A]].show(a)} в коробке"
    }
  }

  object ShowSyntax {
