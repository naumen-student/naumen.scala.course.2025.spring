import utest._

object Test extends TestSuite {

    val tests = Tests {
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 5) == 8)
            assert(Exercises.sumOfDivBy3Or5(1, 1) == 0)
            assert(Exercises.sumOfDivBy3Or5(1, 15) == 60)
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(97) == Seq(97))
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
        }

        'test_sumScalars - {
            import Exercises.Vector2D
            val leftVector0 = Vector2D(1, 0)
            val leftVector1 = Vector2D(1, 1)
            val rightVector0 = Vector2D(1, 0)
            val rightVector1 = Vector2D(1, 1)

            assert(Exercises.sumScalars(leftVector0, leftVector1, rightVector0, rightVector1) == 2.0)
            assert(Exercises.sumScalars(leftVector1, rightVector1, leftVector0, rightVector0) == 3.0)
        }

        'test_sumCosines - {
            import Exercises.Vector2D
            val leftVector0 = Vector2D(1, 0)
            val leftVector1 = Vector2D(1, 1)
            val rightVector0 = Vector2D(1, 0)
            val rightVector1 = Vector2D(1, 1)

            assert(Exercises.sumCosines(leftVector0, leftVector1, rightVector0, rightVector1) == 1.4)
            assert(Exercises.sumCosines(leftVector0, rightVector0, leftVector1, rightVector1) == 2.0)
        }

        'test_sortByHeavyweight - {
            import Exercises.balls

            val expected = Seq(
                "Lithium", "Potassium", "Sodium", "Calcium", "Magnesium",
                "Cesium", "Graphite", "Silver", "Zirconium", "Chrome",
                "Tin", "Iron", "Aluminum", "Cobalt", "Copper", "Nickel",
                "Titanium", "Lead", "Uranium", "Gold", "Plutonium",
                "Tungsten", "Platinum"
            )

            assert(Exercises.sortByHeavyweight(balls) == expected)
        }
    }
}
