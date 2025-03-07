import utest._

object Test extends TestSuite {
    val tests = Tests {
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 10) == 23L)
            assert(Exercises.sumOfDivBy3Or5(1, 100) == 2318L)
            assert(Exercises.sumOfDivBy3Or5(0, 0) == 0L)
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
            assert(Exercises.primeFactor(100) == Seq(2, 5))
            assert(Exercises.primeFactor(17) == Seq(17))
            assert(Exercises.primeFactor(1) == Seq())
        }

        'test_vectorOps - {
            val v1 = Vector2D(1, 2)
            val v2 = Vector2D(3, 4)
            val v3 = Vector2D(-1, 1)
            val v4 = Vector2D(2, -2)

            assert(Exercises.scalar(v1, v2) == 11)
            assert(Math.abs(Exercises.cosBetween(v1, v2) - 0.9701425001453319) < 0.000001)

            assert(Exercises.sumScalars(v1, v2, v3, v4) == 22)
            assert(Math.abs(Exercises.sumCosines(v1, v2, v3, v4) - 1.9402850002906638) < 0.000001)
        }

        'test_sortByHeavyweight - {
            val balls = Map(
                "Aluminum" -> (3, 2.6889),
                "Iron" -> (3, 7.874)
            )

            assert(Exercises.sortByHeavyweight(balls) == Seq("Aluminum", "Iron"))

            // Тесты на полном наборе данных
            val result = Exercises.sortByHeavyweight()
            assert(result.head == "Lithium") // самый легкий шарик
            assert(result.last == "Platinum") // самый тяжелый шарик
        }
    }
}