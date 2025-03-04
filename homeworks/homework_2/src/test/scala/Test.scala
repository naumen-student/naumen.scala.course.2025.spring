import utest._

object Test extends TestSuite {

  val tests = Tests {

    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
      assert(Exercises.sumOfDivBy3Or5(10, 20) == 75)
    }

    'test_primeFactor - {
      assert(Exercises.primeFactor(80) == Seq(2, 5))
      assert(Exercises.primeFactor(98) == Seq(2, 7))
    }

    'test_sumScalars - {
      val vec1 = Exercises.Vector2D(1, 2)
      val vec2 = Exercises.Vector2D(3, 4)
      val vec3 = Exercises.Vector2D(5, 6)
      val vec4 = Exercises.Vector2D(7, 8)
      assert(Exercises.sumScalars(vec1, vec2, vec3, vec4) == 110)
    }

    'test_sumCosines - {
      val vec1 = Exercises.Vector2D(1, 0)
      val vec2 = Exercises.Vector2D(0, 1)
      val vec3 = Exercises.Vector2D(1, 1)
      val vec4 = Exercises.Vector2D(-1, 1)
      assert(Exercises.sumCosines(vec1, vec2, vec3, vec4) == 0.0)
    }

    'test_sortByHeavyweight - {
      val sorted = Exercises.sortByHeavyweight()
      assert(sorted.head == "Lithium")
      assert(sorted.last == "Platinum")
    }
  }
}