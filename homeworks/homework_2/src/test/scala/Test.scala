import utest._
import Exercises._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
            assert(Exercises.sumOfDivBy3Or5(1, 0) == 0)
            assert(Exercises.sumOfDivBy3Or5(0, 0) == 0)
            assert(Exercises.sumOfDivBy3Or5(-10, 10) == 0)
            assert(Exercises.sumOfDivBy3Or5(3, 5) == 8)
            assert(Exercises.sumOfDivBy3Or5(10, 10) == 10)
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
            assert(Exercises.primeFactor(13) == Seq(13))
            assert(Exercises.primeFactor(1) == Seq())
            assert(Exercises.primeFactor(0) == Seq())
            assert(Exercises.primeFactor(-10) == Seq())
            assert(Exercises.primeFactor(36) == Seq(2, 3))
            assert(Exercises.primeFactor(1024) == Seq(2))
        }

        'test_sumScalars - {
            val v1 = Vector2D(1, 2)
            val v2 = Vector2D(3, 4)
            val v3 = Vector2D(5, 6)
            val v4 = Vector2D(7, 8)

            assert(Exercises.sumScalars(v1, v2, v3, v4) == (1*3 + 2*4 + 5*7 + 6*8))

            val zero = Vector2D(0, 0)
            assert(Exercises.sumScalars(zero, zero, zero, zero) == 0)

            val horiz = Vector2D(1, 0)
            val vert = Vector2D(0, 1)
            assert(Exercises.sumScalars(horiz, vert, horiz, vert) == 0)
        }

        'test_sumCosines - {
            val v1 = Vector2D(1, 0)
            val v2 = Vector2D(0, 1)
            val v3 = Vector2D(1, 1)
            val v4 = Vector2D(1, 0)

            val expected = 0.0 + (1.0/math.sqrt(2))
            assert(math.abs(Exercises.sumCosines(v1, v2, v3, v4) - expected) < 1e-6)

            assert(math.abs(Exercises.sumCosines(v1, v1, v2, v2) - 2.0) < 1e-6)

            val v1Opposite = Vector2D(-1, 0)
            assert(math.abs(Exercises.sumCosines(v1, v1Opposite, v1, v1Opposite) - (-2.0)) < 1e-6)

            val zero = Vector2D(0, 0)
            assert(Exercises.sumCosines(zero, v1, zero, v1).isNaN)
        }

        'test_sortByHeavyweight - {
            val result = Exercises.sortByHeavyweight()
            val balls = Exercises.balls

            assert(result.head == "Tin")
            assert(result.last == "Graphite")

            assert(result.length == balls.size)

            assert(result.indexOf("Tin") < result.indexOf("Platinum"))
            assert(result.indexOf("Nickel") < result.indexOf("Gold"))
            assert(result.indexOf("Lead") < result.indexOf("Iron"))
            assert(result.indexOf("Lithium") < result.indexOf("Potassium"))

            val sortedNames = result.toList
            assert(sortedNames.indexOf("Platinum") < sortedNames.indexOf("Aluminum"))
            assert(sortedNames.indexOf("Uranium") < sortedNames.indexOf("Gold"))

            assert(Exercises.sortByHeavyweight(Map.empty).isEmpty)
        }
    }
}