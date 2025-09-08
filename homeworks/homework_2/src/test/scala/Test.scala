import utest._

object Test extends TestSuite {

  val tests = Tests {
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(2, 7) == Seq(3, 6, 7))
      assert(Exercises.divBy3Or7(8, 15) == Seq(9, 12, 14, 15))
    }

    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(2, 4) == 3L)
      assert(Exercises.sumOfDivBy3Or5(4, 10) == 30L)
      assert(Exercises.sumOfDivBy3Or5(11, 11) == 0L)
    }

    'test_primeFactor - {
      assert(Exercises.primeFactor(60) == Seq(2, 3, 5))
      assert(Exercises.primeFactor(77) == Seq(7, 11))
    }

    'test_sumScalars - {
      val a = Exercises.Vector2D(2, 3)
      val b = Exercises.Vector2D(4, 1)
      val c = Exercises.Vector2D(0, 5)
      val d = Exercises.Vector2D(1, 1)
      assert(Exercises.sumScalars(a, b, c, d) == Exercises.scalar(a, b) + Exercises.scalar(c, d))
    }

    'test_sumCosines - {
      val v1 = Exercises.Vector2D(1, 0)
      val v2 = Exercises.Vector2D(1, 0)
      val v3 = Exercises.Vector2D(0, 1)
      val v4 = Exercises.Vector2D(0, 1)
      val expected = 2.0
      assert(math.abs(Exercises.sumCosines(v1, v2, v3, v4) - expected) < 1e-6)
    }

    'test_sortByHeavyweight - {
      val testBalls = Map(
        "Steel"   -> (3, 7.85),  // приблизительно 887.8 единиц массы
        "Plastic" -> (2, 1.5),   // около 50.3
        "Wood"    -> (5, 0.7)    // около 366.5
      )
      assert(Exercises.sortMaterialsByMass(testBalls) == Seq("Plastic", "Wood", "Steel"))
      assert(Exercises.sortMaterialsByMass(Map()) == Seq())
    }
  }
}