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

  object MyEither {
    final case class MyLeft[E](value: E) extends MyEither[E, Nothing] {
      def isError: Boolean = true
    }
    final case class MyRight[A](value: A) extends MyEither[Nothing, A] {
      def isError: Boolean = false
    }

    def apply[A](value: A): MyEither[Nothing, A] = MyRight(value)

    def error[E, A](error: E): MyEither[E, A] = MyLeft(error)

    def possibleError[A](f: => A): MyEither[Throwable, A] =
      try MyRight(f)
      catch { case t: Throwable => MyLeft(t) }

    implicit def myEitherMonad[E]: MonadError[MyEither, E] = new MonadError[MyEither, E] {
      override def pure[A](a: A): MyEither[E, A] = MyRight(a)

      override def flatMap[A, B](fa: MyEither[E, A])(f: A => MyEither[E, B]): MyEither[E, B] =
        fa match {
          case MyLeft(e) => MyLeft(e)
          case MyRight(a) => f(a)
        }

      override def handleError[A](fa: MyEither[E, A])(f: E => A): MyEither[E, A] =
        fa match {
          case MyLeft(e) => MyRight(f(e))
          case r @ MyRight(_) => r
        }

      override def raiseError[A](fa: MyEither[E, A])(error: => E): MyEither[E, A] =
        MyLeft(error)
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
