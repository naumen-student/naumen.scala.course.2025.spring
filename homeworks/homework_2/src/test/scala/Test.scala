import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(0, 15) == 60)
            assert(Exercises.sumOfDivBy3Or5(5, 15) == 57)
            assert(Exercises.sumOfDivBy3Or5(15, 0) == 0)
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(800) == Seq(2, 5))
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
            assert(Exercises.primeFactor(343) == Seq(7))
            assert(Exercises.primeFactor(6) == Seq(2, 3))
        }
        val vector1 = Exercises.Vector2D(0,0)
        val vector2 = Exercises.Vector2D(1,1)
        val vector3 = Exercises.Vector2D(2,2)
        val vector4 = Exercises.Vector2D(3,3)
        'test_sumScalars - {
            assert(Exercises.sumScalars(vector1, vector2, vector3, vector4) == 12)
        }
        val cosVector1 = Exercises.Vector2D(1,0)
        val cosVector2 = Exercises.Vector2D(1,0)
        val cosVector3 = Exercises.Vector2D(0, 1)
        val cosVector4 = Exercises.Vector2D(0, -1)
        'test_sumCosines - {
            assert(Exercises.sumScalars(cosVector1, cosVector2, cosVector3, cosVector4) == 0)
        }

        val balls: Map[String, (Int, Double)] =
            Map(
                "Lead" -> (2,   11.336), "Titanium" ->  (2,   10.50), "Silver" ->    (4,   4.505), "Uranium" ->   (2,   19.04)
            )

        'test_sortByHeavyweight - {
            assert(Exercises.sortByHeavyweight(balls) == Seq("Silver", "Titanium", "Lead", "Uranium"))
        }
    }
}
