import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 2) == 0)
            assert(Exercises.sumOfDivBy3Or5(3, 6) == 14)
            assert(Exercises.sumOfDivBy3Or5(12, 12) == 12)
            assert(Exercises.sumOfDivBy3Or5(-10, 10) == 0)
    	}

        'test_primeFactor - {
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
            assert(Exercises.primeFactor(13) == Seq(13))
            assert(Exercises.primeFactor(1) ==  Seq.empty)
            assert(Exercises.primeFactor(0) ==  Seq.empty)
            assert(Exercises.primeFactor(-10) ==  Seq.empty)
        }

        'test_sumScalars - {
            assert(sumScalars(Vector2D(1, 0), Vector2D(0, 1), Vector2D(1, 1), Vector2D(-1, 1)) == 0)
            assert(sumScalars(Vector2D(2, 3), Vector2D(4, 5), Vector2D(-1, 2), Vector2D(3, -4)) == 12)
        }

        'test_sumCosines - {
            assert(sumCosines(Vector2D(1, 0), Vector2D(0, 1), Vector2D(1, 1), Vector2D(-1, 1)) == 0)
            assert((sumCosines(Vector2D(2, 3), Vector2D(4, 5), Vector2D(-1, 2), Vector2D(3, -4)) - (-0.126)).abs < 1e-6)
        }

        'test_sortByHeavyweight - {
            val ballsTest = Map("Aluminum" -> (3,   2.6889), "Tungsten" ->  (2,   19.35), "Graphite" ->  (12,  2.1))

            assert(Exercises.sortByHeavyweight(ballsTest) == Seq("Aluminum", "Tungsten", "Graphite"))
            assert(Exercises.sortByHeavyweight(Map.empty) == Seq.empty)
        }
}
