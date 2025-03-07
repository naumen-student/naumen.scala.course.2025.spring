import utest._

object Test extends TestSuite {

  val tests = Tests {
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
    }

    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(1, 3) == 3L)
      assert(Exercises.sumOfDivBy3Or5(5, 9) == 20L)
      assert(Exercises.sumOfDivBy3Or5(0, 100) == 2418L)
      assert(Exercises.sumOfDivBy3Or5(0, 0) == 0L)
    }

    'test_sumScalars - {
      val v1 = Exercises.Vector2D(1, 2)
      val v2 = Exercises.Vector2D(3, 4)
      val v3 = Exercises.Vector2D(5, 6)
      val v4 = Exercises.Vector2D(7, 8)
      assert(Exercises.sumScalars(v1, v2, v3, v4) == Exercises.scalar(v1, v2) + Exercises.scalar(v3, v4))
    }

    'test_sumCosines - {
      val v1 = Exercises.Vector2D(1, 0)
      val v2 = Exercises.Vector2D(0, 1)
      val v3 = Exercises.Vector2D(1, 0)
      val v4 = Exercises.Vector2D(0, 1)
      assert(math.abs(Exercises.sumCosines(v1, v2, v3, v4) - 0.0) < 0.0001)
    }

    'test_sortByHeavyweight - {
      val ballsTest = Map(
        "Aluminum" -> (3, 2.6889),
        "Tungsten" -> (2, 19.35),
        "Graphite" -> (12, 2.1),
        "Iron" -> (3, 7.874),
      )
      assert(Exercises.sortByHeavyweight(ballsTest) == Seq("Aluminum", "Tungsten", "Iron", "Graphite"))
      assert(Exercises.sortByHeavyweight(Map()) == Seq())
    }
  }
}
