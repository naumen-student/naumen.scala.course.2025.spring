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
  
  case class MySuccess[+A](value: A) extends MyEither[Nothing, A] {
    def isError: Boolean = false
  }
  
  case class MyFailure[+E](error: E) extends MyEither[E, Nothing] {
    def isError: Boolean = true
  }
  
  object MyEither {
    def apply[A](value: A): MyEither[Nothing, A] = MySuccess(value)
    
    def error[E, A](error: E): MyEither[E, A] = MyFailure(error)
    
    def possibleError[A](f: => A): MyEither[Throwable, A] = {
      try {
        MySuccess(f)
      } catch {
        case e: Throwable => MyFailure(e)
      }
    }

    implicit def myEitherMonad[E]: MonadError[MyEither, E] = new MonadError[MyEither, E] {
      def pure[A](value: A): MyEither[E, A] = MySuccess(value)
      
      def flatMap[A, B](fa: MyEither[E, A])(f: A => MyEither[E, B]): MyEither[E, B] = 
        fa match {
          case MySuccess(value) => f(value)
          case failure: MyFailure[E] => failure
        }
      
      def raiseError[A](fa: MyEither[E, A])(error: => E): MyEither[E, A] = 
        MyFailure(error)
      
      def handleError[A](fa: MyEither[E, A])(handle: E => A): MyEither[E, A] = 
        fa match {
          case MySuccess(_) => fa
          case MyFailure(e) => MySuccess(handle(e))
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
