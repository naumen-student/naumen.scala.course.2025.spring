import utest._

object Test extends TestSuite {

  val tests = Tests {
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(
        0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24,
        27, 28, 30, 33, 35, 36, 39, 42, 45, 48,
        49, 51, 54, 56, 57, 60, 63, 66, 69, 70,
        72, 75, 77, 78, 81, 84, 87, 90, 91, 93,
        96, 98, 99))
    }
    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(0, 0) == 0)
      assert(Exercises.sumOfDivBy3Or5(1, 1) == 0)
      assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
      assert(Exercises.sumOfDivBy3Or5(10, 15) == 37)
    }
    'test_primeFactor - {
      assert(Exercises.primeFactor(0) == Seq())
      assert(Exercises.primeFactor(1) == Seq())
      assert(Exercises.primeFactor(2) == Seq(2))
      assert(Exercises.primeFactor(98) == Seq(2, 7))
      assert(Exercises.primeFactor(-80) == Seq(2, 5))
    }
    'test_sumScalars - {
      val vec1 = Exercises.Vector2D(1, 2)
      val vec2 = Exercises.Vector2D(3, 4)
      val vec3 = Exercises.Vector2D(5, 6)
      val vec4 = Exercises.Vector2D(7, 8)
      val exp = Exercises.scalar(vec1, vec2) + Exercises.scalar(vec3, vec4)
      assert(Exercises.sumScalars(vec1, vec2, vec3, vec4) == exp)
    }
    'test_sumCosines - {
      val vec1 = Exercises.Vector2D(1, 2)
      val vec2 = Exercises.Vector2D(3, 4)
      val vec3 = Exercises.Vector2D(5, 6)
      val vec4 = Exercises.Vector2D(7, 8)
      val exp = Exercises.cosBetween(vec1, vec2) + Exercises.cosBetween(vec3, vec4)
      assert(Exercises.sumCosines(vec1, vec2, vec2, vec3) - exp < 1e-6)
    }
    'test_sortByHeavyweight - {
      val balls1 = Map("Lithium" -> (12, 0.534), "Magnesium" -> (10, 1.738), "Copper" -> (3, 8.96))
      assert(Exercises.sortByHeavyweight(balls1) == Seq("Copper", "Lithium", "Magnesium"))

      val balls2 = Map("Iridium1" -> (1, 2d), "Iridium2" -> (1, 2d))
      assert(Exercises.sortByHeavyweight(balls2) == Seq("Iridium1", "Iridium2"))

      assert(Exercises.sortByHeavyweight(Map.empty) == Seq.empty)
    }
  }
}
