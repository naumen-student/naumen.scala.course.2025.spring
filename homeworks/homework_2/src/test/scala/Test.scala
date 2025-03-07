import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7- {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(0, 3) == 3)
            assert(Exercises.sumOfDivBy3Or5(0, 10) == 33)
            assert(Exercises.sumOfDivBy3Or5(15, 15) == 15)

        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(120) == Seq(2, 3, 5))
            assert(Exercises.primeFactor(45) == Seq(3, 5))
            assert(Exercises.primeFactor(37) == Seq(37))
            assert(Exercises.primeFactor(49) == Seq(7))
        }

        'test_sumByFunc - {
            val v1 = Exercises.Vector2D(2, 3)
            val v2 = Exercises.Vector2D(1, 4)
            val v3 = Exercises.Vector2D(5, 6)
            val v4 = Exercises.Vector2D(7, 8)
            val expectedScalarSum = Exercises.scalar(v1, v2) + Exercises.scalar(v3, v4)
            val expectedCosineSum = Exercises.cosBetween(v1, v2) + Exercises.cosBetween(v3, v4)
            assert(Exercises.sumScalars(v1, v2, v3, v4) == expectedScalarSum)
            assert(Exercises.sumCosines(v1, v2, v3, v4) == expectedCosineSum)
        }

        'test_sortByHeavyweight - {
            val ballsTest = Map(
                "Plutonium" -> (5, 19.25),
                "Silver" -> (4, 4.505),
                "Copper" -> (3, 8.96),
                "Aluminum" -> (2, 2.6889)
            )

            val sorted = Exercises.sortByHeavyweight(ballsTest)
            assert(sorted == Seq("Aluminum", "Silver", "Copper", "Plutonium"))
    }
}
