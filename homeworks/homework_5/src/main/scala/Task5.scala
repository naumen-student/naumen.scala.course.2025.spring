import scala.util.{Failure, Success, Try}

object Task5 extends App {
  import Task4.MonadError

  sealed trait MyEither[+E, +A] {
    def isError: Boolean

    def value: Either[E, A]
  }

  object MyEither {
    final case class MyRight[+A](result: A) extends MyEither[Nothing, A] {
      def isError: Boolean = false
      def value: Either[Nothing, A] = Right(result)
    }

    final case class MyLeft[+E](error: E) extends MyEither[E, Nothing] {
      def isError: Boolean = true
      def value: Either[E, Nothing] = Left(error)
    }

    def apply[A](value: A): MyEither[Nothing, A] = MyRight(value)

    def error[E, A](error: E): MyEither[E, A] = MyLeft(error)

    def possibleError[A](f: => A): MyEither[Throwable, A] = {
      Try(f) match {
        case Success(value) => MyRight(value)
        case Failure(error) => MyLeft(error)
      }
    }

    implicit def myEitherMonad[E]: MonadError[MyEither, E] = new MonadError[MyEither, E] {
      override def pure[A](value: A): MyEither[E, A] = MyRight(value)

      override def flatMap[A, B](fa: MyEither[E, A])(f: A => MyEither[E, B]): MyEither[E, B] = fa match {
        case MyRight(value) => f(value)
        case e @ MyLeft(_)  => e
      }

      override def map[A, B](fa: MyEither[E, A])(f: A => B): MyEither[E, B] =
        flatMap(fa)(a => pure(f(a)))

      override def raiseError[A](error: => E): MyEither[E, A] = MyLeft(error)

      override def handleError[A](fa: MyEither[E, A])(handle: E => A): MyEither[E, A] = fa match {
        case MyRight(value) => MyRight(value)
        case MyLeft(e)      => MyRight(handle(e))
      }
    }
  }

  object MyEitherSyntax {
    implicit class MyEitherOps[E, A](val either: MyEither[E, A]) {
      def flatMap[B](f: A => MyEither[E, B]): MyEither[E, B] =
        MyEither.myEitherMonad[E].flatMap(either)(f)

      def map[B](f: A => B): MyEither[E, B] =
        MyEither.myEitherMonad[E].map(either)(f)

      def handleError(f: E => A): MyEither[E, A] =
        MyEither.myEitherMonad[E].handleError(either)(f)
    }
  }
}
