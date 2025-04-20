import utest._

import scala.util.Random

object Test5 extends TestSuite {
  override def tests: Tests = Tests {
    import Task5._
    import Task5.MyEitherSyntax._

    'leftIdentityLow - {
      assert(MyEither(0).flatMap(x => MyEither(x + 1)) == MyEither(1))
    }
    'rightIdentityLow - {
      assert(MyEither(0).flatMap(x => MyEither.apply(x)) == MyEither(0))
    }
    'associativityLow - {
      val f: Int => MyEither[Nothing, Int] = x => MyEither(x + 1)
      val g: Int => MyEither[Nothing, Int] = x => MyEither(x * 10)
      assert(MyEither(0).flatMap(f).flatMap(g) == MyEither(0).flatMap(x => f(x).flatMap(g)))
    }
    'usage - {
      'simpleUsage - {
        val (x, y, z) = (Random.nextInt(), Random.nextInt(), Random.nextInt())
        val p = for {
          a <- MyEither(x)
          c <- MyEither(y)
          d <- MyEither(z)
        } yield a + c + d
        assert(p == MyEither(x + y + z))
      }
      'errorUsage - {
        val p = for {
          a <- MyEither(0)
          _ <- MyEither.error[String, Int]("Error")
        } yield a
        assert(p.isError)
      }
      'possibleErrorUsage - {
        val p = for {
          a <- MyEither(12)
          b <- MyEither.possibleError(12 / 0)
        } yield a + b
        assert(p.isError)
      }
      'recoverErrorUsage - {
        val (x, y, z) = (Random.nextInt(), Random.nextInt(), Random.nextInt())
        val p = for {
          a <- MyEither(x)
          b <- MyEither.possibleError(y / 0).handleError(_ => y)
          c <- MyEither(z)
        } yield a + b + c

        assert(p == MyEither(x + y + z))
      }
      'skippingErrorStopsEvaluation - {
        val p = for {
          a <- MyEither(0)
          v <- MyEither.possibleError(12 / 0)
          b <- MyEither.possibleError(32 / 12).handleError(_ => 1)
          c <- MyEither(2)
        } yield a + b + c + v
        assert(p.isError)
      }
    }
  }
}
