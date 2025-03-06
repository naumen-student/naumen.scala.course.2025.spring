import utest._
import Exercises.Vector2D

object Test extends TestSuite{

  val tests = Tests{
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
    }

    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
      assert(Exercises.sumOfDivBy3Or5(13, 15) == 15)
      assert(Exercises.sumOfDivBy3Or5(0, 20) == 98)
    }

    'test_primeFactor - {
      assert(Exercises.primeFactor(7) == Seq(7))
      assert(Exercises.primeFactor(80) == Seq(2, 5))
      assert(Exercises.primeFactor(98) == Seq(2, 7))
    }

    'test_sumScalars - {
      val v1 = Vector2D(1, 0)
      val v2 = Vector2D(8, 8)
      val v3 = Vector2D(5, 2)
      val v4 = Vector2D(6, 10)

      assert(Exercises.sumScalars(v1, v2, v3, v4) == 58.0)
      assert(Exercises.sumScalars(v1, v4, v2, v3) == 62.0)
      assert(Exercises.sumScalars(v2, v4, v2, v4) == 256.0)

    }

    'test_sumCosines - {
      val v1 = Vector2D(1, 0)
      val v2 = Vector2D(5, 0)
      val v3 = Vector2D(4, 3)
      val v4 = Vector2D(4, 3)

      assert(Exercises.sumCosines(v1, v2, v1, v2) == 2.0)
      assert(Exercises.sumCosines(v3, v4, v4, v3) == 2.0)

    }

    'test_sortByHeavyweight - {
      val balls_1 = Map(
        "Aluminum" -> (3,   2.6889),
        "Tungsten" -> (2,   19.35),
        "Graphite" -> (12,  2.1)
      )
      val balls_2 = Map(
        "Gold" -> (2,   19.32),
        "Cobalt" -> (4,   8.90),
        "Magnesium" -> (10,  1.738),
        "Titanium" -> (2,   10.50),
      )
      val balls_3 = Map(
        "Nickel" -> (2,   8.91),
        "Tin" -> (1,   7.29),
        "Platinum" -> (1,   21.45),
        "Lead" -> (2,   11.336),
        "Silver" -> (4,   4.505),
      )

      assert(Exercises.sortByHeavyweight(balls_1) == Seq("Aluminum", "Tungsten", "Graphite"))
      assert(Exercises.sortByHeavyweight(balls_2) == Seq("Titanium", "Gold", "Cobalt", "Magnesium"))
      assert(Exercises.sortByHeavyweight(balls_3) == Seq("Tin", "Platinum", "Nickel", "Lead", "Silver"))
    }
  }
}
