import Exercises.{Vector2D, sortByHeavyweight}
import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(0, 10) == 33)
            assert(Exercises.sumOfDivBy3Or5(-10, 0) == -33)
            assert(Exercises.sumOfDivBy3Or5(0, 100) == 2418)
        }
        'test_primeFactor {
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
            assert(Exercises.primeFactor(9) == Seq(3))
            assert(Exercises.primeFactor(2) == Seq(2))
            assert(Exercises.primeFactor(-10) == Seq.empty)
        }
        'test_sumScalars {
            val vecA = Vector2D(1.0, 2.0)
            val vecB = Vector2D(3.0, 4.0)
            val vecC = Vector2D(5.0, 6.0)
            val vecD = Vector2D(7.0, 8.0)

            val result = Exercises.sumScalars(vecA, vecB, vecC, vecD)
            assert(result == Exercises.scalar(vecA, vecB) + Exercises.scalar(vecC, vecD))
        }
        'test_sumCosines {
            val vecA = Vector2D(1.0, 0.0)
            val vecB = Vector2D(0.0, 1.0)
            val vecC = Vector2D(1.0, 1.0)
            val vecD = Vector2D(-1.0, -1.0)

            val result = Exercises.sumCosines(vecA, vecB, vecC, vecD)
            assert(result == Exercises.cosBetween(vecA, vecB) + Exercises.cosBetween(vecC, vecD))
        }
        'test_sortByHeavyweight {
            val result: Seq[String] = Seq(
                "Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead", "Sodium", "Uranium", "Gold",
                "Tungsten", "Zirconium", "Chrome", "Iron", "Copper", "Silver", "Plutonium", "Cobalt", "Cesium",
                "Calcium", "Lithium", "Magnesium", "Potassium", "Graphite"
            )

            assert(sortByHeavyweight() == result)
        }
    }
}
