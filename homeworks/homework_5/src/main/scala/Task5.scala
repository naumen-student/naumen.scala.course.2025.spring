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
  
  // Аналог Right для успешного результата
  final case class MyRight[+A](value: A) extends MyEither[Nothing, A] {
    override def isError: Boolean = false
  }
  
  // Аналог Left для ошибки
  final case class MyLeft[+E](error: E) extends MyEither[E, Nothing] {
    override def isError: Boolean = true
  }
  
  object MyEither {
    def apply[A](value: A): MyEither[Nothing, A] = MyRight(value)
    
    def error[E, A](error: E): MyEither[E, A] = MyLeft(error)
    
    def possibleError[A](f: => A): MyEither[Throwable, A] = {
      try {
        MyEither(f)
      } catch {
        case e: Throwable => error(e)
      }
    }

    implicit def myEitherMonad[E]: MonadError[MyEither, E] = new MonadError[MyEither, E] {
      def pure[A](value: A): MyEither[E, A] = MyRight(value)
      
      def flatMap[A, B](fa: MyEither[E, A])(f: A => MyEither[E, B]): MyEither[E, B] = 
        fa match {
          case MyRight(a) => f(a)
          case MyLeft(e) => MyLeft(e)
        }
      
      def raiseError[A](fa: MyEither[E, A])(error: => E): MyEither[E, A] = 
        MyLeft(error)
      
      def handleError[A](fa: MyEither[E, A])(handle: E => A): MyEither[E, A] = 
        fa match {
          case MyRight(a) => MyRight(a)
          case MyLeft(e) => MyRight(handle(e))
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
