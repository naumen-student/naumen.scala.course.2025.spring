import utest._

import scala.util.Random

object Test4 extends TestSuite {
  override def tests: Tests = Tests {
    import Task4._
    import Task4.EIOSyntax._

    'leftIdentityLow - {
      assert(EIO(0).flatMap(x => EIO(x + 1)) == EIO(1))
    }
    'rightIdentityLow - {
      assert(EIO(0).flatMap(x => EIO.apply(x)) == EIO(0))
    }
    'associativityLow - {
      val f: Int => EIO[Nothing, Int] = x => EIO(x + 1)
      val g: Int => EIO[Nothing, Int] = x => EIO(x * 10)
      assert(EIO(0).flatMap(f).flatMap(g) == EIO(0).flatMap(x => f(x).flatMap(g)))
    }
    'usage - {
      'simpleUsage - {
        val (x, y, z) = (Random.nextInt(), Random.nextInt(), Random.nextInt())
        val p = for {
          a <- EIO(x)
          c <- EIO(y)
          d <- EIO(z)
        } yield a + c + d
        assert(p == EIO(Right(x + y + z)))
      }
      'errorUsage - {
        val p = for {
          a <- EIO(0)
          _ <- EIO.error[String, Int]("Error")
        } yield a
        assert(p == EIO(Left("Error")))
      }
      'possibleErrorUsage - {
        val p = for {
          a <- EIO(12)
          b <- EIO.possibleError(12 / 0)
        } yield a + b
        assert(p.value.isLeft)
      }
      'recoverErrorUsage - {
        val (x, y, z) = (Random.nextInt(), Random.nextInt(), Random.nextInt())
        val p = for {
          a <- EIO(x)
          b <- EIO.possibleError(y / 0).handleError(_ => y)
          c <- EIO(z)
        } yield a + b + c

        assert(p == EIO(Right(x + y + z)))
      }
      'skippingErrorStopsEvaluation - {
        val p = for {
          a <- EIO(0)
          v <- EIO.possibleError(12 / 0)
          b <- EIO.possibleError(32 / 12).handleError(_ => 1)
          c <- EIO(2)
        } yield a + b + c + v
        assert(p.value.isLeft)
      }
    }
  }
}
