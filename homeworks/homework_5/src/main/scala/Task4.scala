import scala.language.higherKinds
import scala.util.{Failure => TryFailure, Success => TrySuccess, Try}

object Task4 {
  trait MonadError[F[_, _], E] {
    def pure[A](value: A): F[E, A]
    def flatMap[A, B](fa: F[E, A])(f: A => F[E, B]): F[E, B]

    def map[A, B](fa: F[E, A])(f: A => B): F[E, B] =
      flatMap(fa)(a => pure(f(a)))

    def raiseError[A](error: => E): F[E, A]

    def handleError[A](fa: F[E, A])(handler: E => A): F[E, A]
  }

  case class EIO[+E, +A](value: Either[E, A])

  object EIO {
    def apply[A](value: A): EIO[Nothing, A] = EIO(Right(value))

    def error[E, A](error: E): EIO[E, A] = EIO(Left(error))

    def possibleError[A](expr: => A): EIO[Throwable, A] =
      Try(expr) match {
        case TrySuccess(v) => EIO(v)
        case TryFailure(e) => EIO.error(e)
      }

    implicit def monad[E]: MonadError[EIO, E] = new MonadError[EIO, E] {
      override def pure[A](value: A): EIO[E, A] = EIO(Right(value))

      override def flatMap[A, B](fa: EIO[E, A])(f: A => EIO[E, B]): EIO[E, B] =
        fa.value match {
          case Right(a) => f(a)
          case Left(e)  => EIO(Left(e))
        }

      override def raiseError[A](error: => E): EIO[E, A] =
        EIO(Left(error))

      override def handleError[A](fa: EIO[E, A])(handler: E => A): EIO[E, A] =
        fa.value match {
          case Right(v) => EIO(Right(v))
          case Left(e)  => EIO(Right(handler(e)))
        }
    }
  }

  object EIOSyntax {
    implicit class EIOOps[E, A](val eio: EIO[E, A]) extends AnyVal {
      def flatMap[B](f: A => EIO[E, B]): EIO[E, B] =
        EIO.monad[E].flatMap(eio)(f)

      def map[B](f: A => B): EIO[E, B] =
        EIO.monad[E].map(eio)(f)

      def handleError(f: E => A): EIO[E, A] =
        EIO.monad[E].handleError(eio)(f)
    }
  }
}
