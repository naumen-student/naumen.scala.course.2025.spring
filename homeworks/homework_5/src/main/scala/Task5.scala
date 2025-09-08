import scala.util.{Failure, Success, Try}

/*
  Задание №5
  Задание аналогично предыдущему задания, но теперь мы уходим от использования стандартного Either.
  Нужно:
  1) Доделать реализацию MyEither (нужны аналоги Right и Left)
  2) Написать для MyEither инстанс MonadError
  3) Написать функции apply, error, possibleError
 */
object Task5 extends App {
  import Task4.MonadError

  sealed trait MyEither[+E, +A] {
    def isError: Boolean
  }
  case class MyRight[+E, +A](value: A) extends MyEither[E, A] {
    override def isError: Boolean = false
  }
  case class MyLeft[+E, +A](error: E) extends MyEither[E, A] {
    override def isError: Boolean = true
  }

  object MyEither {
    def apply[A](value: A): MyEither[Nothing, A] = MyRight[Nothing, A](value)

    def error[E, A](error: E): MyEither[E, A] = MyLeft[E, A](error)

    def possibleError[A](f: => A): MyEither[Throwable, A] = Try(f) match {
      case Success(a) => MyRight[Throwable, A](a)
      case Failure(e) => MyLeft[Throwable, A](e)
    }

    implicit def myEitherMonad[E]: MonadError[MyEither, E] = new MonadError[MyEither, E] {
      override def pure[A](value: A): MyEither[E, A] = MyRight[E, A](value)

      override def flatMap[A, B](fa: MyEither[E, A])(f: A => MyEither[E, B]): MyEither[E, B] = fa match {
        case MyRight(a) => f(a)
        case MyLeft(e) => MyLeft[E, B](e)
      }

      override def raiseError[A](fa: MyEither[E, A])(error: => E): MyEither[E, A] = MyLeft[E, A](error)

      override def handleError[A](fa: MyEither[E, A])(handle: E => A): MyEither[E, A] = fa match {
        case MyRight(a) => fa
        case MyLeft(e) => MyRight[E, A](handle(e))
      }
    }
  }

  object MyEitherSyntax {
    implicit class MyEitherOps[E, A](val either: MyEither[E, A]) {
      def flatMap[B](f: A => MyEither[E, B]): MyEither[E, B] =
        MyEither.myEitherMonad[E].flatMap(either)(f)

      def map[B](f: A => B): MyEither[E, B] = MyEither.myEitherMonad.map(either)(f)

      def handleError(f: E => A): MyEither[E, A] =
        MyEither.myEitherMonad.handleError(either)(f)
    }
  }
}
