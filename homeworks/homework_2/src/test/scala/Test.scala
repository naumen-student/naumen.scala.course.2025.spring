import utest._

object Test extends TestSuite{

    val tests: Tests = Tests {
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))

            assert(Exercises.divBy3Or7(0, 0) == Seq(0))
            assert(Exercises.divBy3Or7(-10, 0) == Seq(-9, -7, -6, -3, 0))
            assert(Exercises.divBy3Or7(4, 5) == Seq())
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
            assert(Exercises.sumOfDivBy3Or5(10, 15) == 37)
            assert(Exercises.sumOfDivBy3Or5(0, 100) == 2418)

            assert(Exercises.sumOfDivBy3Or5(0, 0) == 0)
            assert(Exercises.sumOfDivBy3Or5(4, 4) == 0)
            assert(Exercises.sumOfDivBy3Or5(-10, 0) == -33)
            assert(Exercises.sumOfDivBy3Or5(3, 3) == 3)
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(0) == Seq())
            assert(Exercises.primeFactor(1) == Seq())
            assert(Exercises.primeFactor(2) == Seq(2))
            assert(Exercises.primeFactor(80) == Seq(2,5))
            assert(Exercises.primeFactor(-80) == Seq(2,5))
            assert(Exercises.primeFactor(98) == Seq(2,7))
            assert(Exercises.primeFactor(-98) == Seq(2,7))
        }

        'test_sumScalars - {
            val vec1 = Exercises.Vector2D(1, 2)
            val vec2 = Exercises.Vector2D(3, 4)
            val vec3 = Exercises.Vector2D(5, 6)

            val result = Exercises.sumScalars(vec1, vec2, vec2, vec3)
            assert(math.abs(result - (Exercises.scalar(vec1, vec2) + Exercises.scalar(vec2, vec3))) < 1e-6)
        }
        'test_sumCosines - {
            val vec1 = Exercises.Vector2D(1, 2)
            val vec2 = Exercises.Vector2D(3, 4)
            val vec3 = Exercises.Vector2D(5, 6)

            val result = Exercises.sumCosines(vec1, vec2, vec2, vec3)
            assert(math.abs(result - (Exercises.cosBetween(vec1, vec2) + Exercises.cosBetween(vec2, vec3))) < 1e-6)
        }

        'test_sortByHeavyweight - {
            val expectedOrder = Seq(
                "Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead", "Sodium", "Uranium", "Gold", "Tungsten",
                "Zirconium", "Chrome", "Iron", "Copper", "Silver", "Plutonium", "Cobalt", "Cesium", "Calcium", "Lithium",
                "Magnesium", "Potassium", "Graphite"
            )

            val actualOrder = Exercises.sortByHeavyweight()

            assert(actualOrder == expectedOrder)
        }
    }
}