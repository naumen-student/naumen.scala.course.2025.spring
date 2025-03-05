import Exercises.Vector2D
import utest._

object Test extends TestSuite{

    val tests: Tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 3) == 3)
            assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
            assert(Exercises.sumOfDivBy3Or5(11, 11) == 0)
            assert(Exercises.sumOfDivBy3Or5(10, 5) == 0)
            assert(Exercises.sumOfDivBy3Or5(1, 20) == 98)
        }
        'test_primeFactor - {
            assert(Exercises.primeFactor(17) == Seq(17))
            assert(Exercises.primeFactor(9) == Seq(3))
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
            assert(Exercises.primeFactor(210) == Seq(2, 3, 5, 7))
        }
        'test_sumScalars - {
            val vector1 = Vector2D(1, 0)
            val vector2 = Vector2D(0, 1)
            val vector3 = Vector2D(2, 5)
            val vector4 = Vector2D(3, 2)

            assert(Exercises.sumScalars(vector1, vector2, vector3, vector4) == 16)
            assert(Exercises.sumScalars(vector1, vector2, vector1, vector2) == 0)
            assert(Exercises.sumScalars(vector1, vector3, vector2, vector4) == 4)
        }
        'test_sumCosines - {
            val vector1 = Vector2D(1, 0)
            val vector2 = Vector2D(0, 1)
            val vector3 = Vector2D(-1, 0)
            val vector4 = Vector2D(0, -1)

            assert(Exercises.sumCosines(vector1, vector2, vector1, vector2) == 0)
            assert(Exercises.sumCosines(vector3, vector4, vector3, vector4) == 0)
            assert(Exercises.sumCosines(vector3, vector1, vector3, vector1) == -2)
        }
        'test_sortByHeavyweight - {
            val testData = Map(
                "Y" -> (1, 1.111D),
                "X" -> (1, 1D),
                "Z" -> (2, 2.222D)
            )
            assert(Exercises.sortByHeavyweight(testData) == Seq("X", "Y", "Z"))
            assert(Exercises.sortByHeavyweight(Map.empty) == Seq())
        }
    }
}
