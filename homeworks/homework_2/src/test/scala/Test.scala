import utest._
import Exercises._

object Test extends TestSuite {
  val tests = Tests {

    // задание 1
    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(1, 3) == 3L)
      assert(Exercises.sumOfDivBy3Or5(5, 9) == 20L)
      assert(Exercises.sumOfDivBy3Or5(0, 0) == 0L)
      assert(Exercises.sumOfDivBy3Or5(10, 20) == 75L)
    }

    // задание 2
    'test_primeFactor - {
      assert(Exercises.primeFactor(80) == Seq(2, 5))
      assert(Exercises.primeFactor(98) == Seq(2, 7))
      assert(Exercises.primeFactor(1) == Seq.empty)
      assert(Exercises.primeFactor(7) == Seq(7))
      // для отрицательных чисел берем абсолютное значение
      assert(Exercises.primeFactor(-80) == Seq(2, 5))
    }

    // задание 3
    'test_vectors - {
      val vRight = Vector2D(1, 0)
      val vUp = Vector2D(0, 1)
      
      'test_abs - {
        assert(math.abs(Exercises.abs(vRight) - 1.0) < 1e-10)
        assert(math.abs(Exercises.abs(vUp) - 1.0) < 1e-10)
      }
      
      'test_scalar - {
        assert(Exercises.scalar(vRight, vRight) == 1.0)
        assert(Exercises.scalar(vRight, vUp) == 0.0)
      }
      
      'test_cosBetween - {
        assert(math.abs(Exercises.cosBetween(vRight, vRight) - 1.0) < 1e-10)
        assert(math.abs(Exercises.cosBetween(vRight, vUp)) < 1e-10)
      }

      'sumScalars - {
        assert(Exercises.sumScalars(vRight, vRight, vUp, vUp) == 2.0)
        assert(Exercises.sumScalars(vRight, vUp, vUp, vRight) == 0.0)
      }

      'sumCosines - {
        assert(Exercises.sumCosines(vRight, vUp, vUp, vRight) == 0.0)
        assert(Exercises.sumCosines(vRight, vRight, vUp, vUp) == 2.0)
      }
    }

    // задание 4
    'test_sortByHeavyweight - {
      val testBalls = Map(
        "A" -> (1, 10.0),
        "B" -> (3, 1.0),
        "C" -> (2, 5.0)
      )
      
      val result = Exercises.sortByHeavyweight(testBalls)
      assert(result.toSet == Set("A", "B", "C"))
      assert(Exercises.sortByHeavyweight(Map.empty) == Seq.empty)
      
      val realBallsResult = Exercises.sortByHeavyweight()
      assert(realBallsResult.last == "Potassium" || realBallsResult.last == "Lithium" || 
             realBallsResult.last == "Graphite")
    }
  }
}