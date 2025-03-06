import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(10, 15) == 37)
            assert(Exercises.sumOfDivBy3Or5(0, 9) == 23)
        }

        'test_primeFactor' - {
            assert(Exercises.primeFactor(10) == Seq(2, 5))
            assert(Exercises.primeFactor(100) == Seq(2, 2, 5, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7, 7))
        }

        'test_sumScalars'- {
            assert(Exercises.sumScalars(Vector2D(1, 3), Vector2D(5, 7), Vector2D(9, 11), Vector2D(13, 15)) == 308)
            assert(Exercises.sumScalars(Vector2D(2, 4), Vector2D(6, 8), Vector2D(10, 12), Vector2D(14, 16)) == 376)
        }

        'test_sumCosines'- {
            assert(Exercises.sumCosines(Vector2D(1, 3), Vector2D(5, 7), Vector2D(9, 11), Vector2D(13, 15)) == 1.9553768572)
            assert(Exercises.sumCosines(Vector2D(2, 4), Vector2D(6, 8), Vector2D(10, 12), Vector2D(14, 16)) == 1.98357971857)
        }

        val testMap = Map("Al" -> (100, 2.7), "SmthElse" -> (10, 2.5))

        'test_sortByHeavyWeight' - {
            assert(Exercises.sortByHeavyweight(testMap) == Seq("SmthElse", "Al"))
        }
    }
}
