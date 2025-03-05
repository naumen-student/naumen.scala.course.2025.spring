import utest._

object Test extends TestSuite {

  val tests = Tests {
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(
        Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21,
          24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60,
          63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99)
      )
    }

    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(1, 10) == 33L)
      assert(Exercises.sumOfDivBy3Or5(10, 1) == 0L)
      assert(Exercises.sumOfDivBy3Or5(15, 15) == 15L)
      assert(
        Exercises.sumOfDivBy3Or5(
          Int.MaxValue - 1,
          Int.MaxValue
        ) == Int.MaxValue - 1
      )
    }

    'test_primeFactor - {
      assert(Exercises.primeFactor(98) == Seq(2, 7))

      assert(Exercises.primeFactor(13) == Seq(13))

      assert(Exercises.primeFactor(16) == Seq(2))
      
      assert(Exercises.primeFactor(1) == Seq())
      assert(Exercises.primeFactor(0) == Seq())
      assert(Exercises.primeFactor(-80) == Seq(2, 5))
    }

    'test_sumScalars - {
      val v1 = Exercises.Vector2D(1, 2)
      val v2 = Exercises.Vector2D(3, 4)
      val v3 = Exercises.Vector2D(5, 6)
      val v4 = Exercises.Vector2D(7, 8)
      val expected = Exercises.scalar(v1, v2) + Exercises.scalar(v3, v4)
      assert(Exercises.sumScalars(v1, v2, v3, v4) == expected)
    }

    'test_sumScalars_negative - {
      val v1 = Exercises.Vector2D(-1, -2)
      val v2 = Exercises.Vector2D(-3, -4)
      val v3 = Exercises.Vector2D(2, -3)
      val v4 = Exercises.Vector2D(-4, 5)
      val expected = Exercises.scalar(v1, v2) + Exercises.scalar(v3, v4)
      assert(Exercises.sumScalars(v1, v2, v3, v4) == expected)
    }

    'test_sumCosines - {
      val v1 = Exercises.Vector2D(1, 0)
      val v2 = Exercises.Vector2D(1, 0)
      val v3 = Exercises.Vector2D(0, 1)
      val v4 = Exercises.Vector2D(0, 1)
      // Both cosBetween calls should be 1.0, hence the sum is 2.0
      assert(math.abs(Exercises.sumCosines(v1, v2, v3, v4) - 2.0) < 1e-6)
    }

    'test_sumCosines_perpendicular - {
      val v1 = Exercises.Vector2D(1, 0)
      val v2 = Exercises.Vector2D(0, 1)
      val v3 = Exercises.Vector2D(1, 0)
      val v4 = Exercises.Vector2D(0, 1)
      // For perpendicular vectors, cosBetween returns 0.0 so the sum is 0.0
      assert(math.abs(Exercises.sumCosines(v1, v2, v3, v4)) < 1e-6)
    }

    'test_sumCosines_mixed - {
      val v1 = Exercises.Vector2D(1, 0)
      val v2 = Exercises.Vector2D(-1, 0)
      val v3 = Exercises.Vector2D(0, 1)
      val v4 = Exercises.Vector2D(0, -1)
      // In both cases the cosine is -1.0, so the sum is -2.0
      assert(math.abs(Exercises.sumCosines(v1, v2, v3, v4) + 2.0) < 1e-6)
    }

    'test_sumCosines_withZero - {
      val vZero = Exercises.Vector2D(0, 0)
      val vNonZero = Exercises.Vector2D(1, 1)
      val v1 = Exercises.Vector2D(1, 0)
      val v2 = Exercises.Vector2D(1, 0)
      // cosBetween(vZero, vNonZero) yields NaN (since division by zero occurs)
      assert(Exercises.cosBetween(vZero, vNonZero).isNaN)
      // Thus, the sumCosines result will also be NaN when one pair contains a zero vector
      assert(Exercises.sumCosines(vZero, vNonZero, v1, v2).isNaN)
    }


    'test_sortByHeavyweight - {
      val ballsTest = Map(
        "Silver" -> (4, 4.505),
        "Gold"   -> (3, 19.32),
        "Iron"   -> (5, 7.87)
      )
      assert(Exercises.sortByHeavyweight(ballsTest) == Seq("Silver", "Gold", "Iron"))

      assert(Exercises.sortByHeavyweight(Map.empty) == Seq.empty)

      val ballsEqual = Map(
        "A" -> (2, 1.0),
        "B" -> (2, 1.0)
      )
      assert(Exercises.sortByHeavyweight(ballsEqual) == Seq("A", "B"))
    }
  }
}
