import utest._

object Test extends TestSuite{

  val tests = Tests{
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
    }

    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
      assert(Exercises.sumOfDivBy3Or5(10, 20) == 98)
      assert(Exercises.sumOfDivBy3Or5(0, 5) == 8)
    }

    'test_primeFactor - {
      assert(Exercises.primeFactor(80) == Seq(2, 5))
      assert(Exercises.primeFactor(13) == Seq(13))
      assert(Exercises.primeFactor(-98) == Seq(2, 7))
    }

    'test_sumScalars - {
      import Exercises.Vector2D
      val v1 = Vector2D(1, 0)
      val v2 = Vector2D(0, 1)
      val v3 = Vector2D(2, 2)
      val v4 = Vector2D(3, 4)

      assert(Exercises.sumScalars(v1, v3, v2, v4) == 8.0)
      assert(Exercises.sumScalars(v1, v1, v2, v2) == 2.0)
      assert(Exercises.sumScalars(v3, v4, v3, v4) == 28.0)
    }

    'test_sumCosines - {
      import Exercises.Vector2D
      val v1 = Vector2D(1, 0)
      val v2 = Vector2D(0, 1)
      val v3 = Vector2D(1, 1)
      val v4 = Vector2D(-1, 1)

      assert(math.abs(Exercises.sumCosines(v1, v3, v2, v4) - 0.0) < 0.001)
      assert(math.abs(Exercises.sumCosines(v1, v2, v3, v4) - 0.0) < 0.001)
      assert(math.abs(Exercises.sumCosines(v1, v4, v2, v3) - 0.0) < 0.001)
    }

    'test_sortByHeavyweight - {
      val testBalls1 = Map(
        "Lead" -> (2, 11.336),
        "Aluminum" -> (3, 2.6889),
        "Gold" -> (1, 19.32)
      )
      assert(Exercises.sortByHeavyweight(testBalls1) == Seq("Gold", "Aluminum", "Lead"))

      val testBalls2 = Map(
        "A" -> (2, 5.0),
        "B" -> (2, 5.0),
        "C" -> (1, 40.0)
      )
      assert(Exercises.sortByHeavyweight(testBalls2).contains("C"))
      assert(Exercises.sortByHeavyweight(testBalls2).indexOf("A") == Exercises.sortByHeavyweight(testBalls2).indexOf("B") - 1 ||
        Exercises.sortByHeavyweight(testBalls2).indexOf("A") == Exercises.sortByHeavyweight(testBalls2).indexOf("B") + 1)

      val fullResult = Exercises.sortByHeavyweight()
      assert(fullResult.take(3).toSet.subsetOf(Set("Lithium", "Potassium", "Sodium")))
      assert(fullResult.takeRight(3).toSet.subsetOf(Set("Tungsten", "Plutonium", "Uranium", "Platinum")))
    }
  }
}