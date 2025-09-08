import utest._

object Test extends TestSuite{
    val tests = Tests{
        test("test_divBy3Or7") - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        test("test_sumOfDivBy3Or5") {
            assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
            assert(Exercises.sumOfDivBy3Or5(10, 20) == 75)
            assert(Exercises.sumOfDivBy3Or5(1, 2) == 0)
        }
        test("test_primeFactor") - {
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
            assert(Exercises.primeFactor(37) == Seq(37))
            assert(Exercises.primeFactor(1) == Seq())
            assert(Exercises.primeFactor(60) == Seq(2, 3, 5))
            assert(Exercises.primeFactor(100) == Seq(2, 5))
        }

        test("test_sumScalars") {
            val v1 = Exercises.Vector2D(1, 2)
            val v2 = Exercises.Vector2D(3, 4)
            val v3 = Exercises.Vector2D(5, 6)
            val v4 = Exercises.Vector2D(7, 8)
            val result = Exercises.sumScalars(v1, v2, v3, v4)
            // Используем == для сравнения Double
            assert(result == Exercises.scalar(v1, v2) + Exercises.scalar(v3, v4))
        }

        test("test_sumCosines") {
            val v1 = Exercises.Vector2D(1, 0)
            val v2 = Exercises.Vector2D(0, 1)
            val v3 = Exercises.Vector2D(1, 1)
            val v4 = Exercises.Vector2D(-1, -1)
            val result = Exercises.sumCosines(v1, v2, v3, v4)
            // Используем == для сравнения Double
            assert(result == Exercises.cosBetween(v1, v2) + Exercises.cosBetween(v3, v4))
        }
        test("test_sortByHeavyweight") {
            val ballsTest: Map[String, (Int, Double)] =
                Map(
                    "Aluminum" -> (3,   2.6889), "Graphite" ->  (12,  2.1),   "Iron" ->      (3,   7.874),
                    "Magnesium" -> (10,  1.738), "Titanium" ->  (2,   10.50), "Silver" ->    (4,   4.505),
                )

            val expectedListBalls = Seq("Aluminum", "Titanium", "Iron", "Silver", "Magnesium", "Graphite" )
            assert(Exercises.sortByHeavyweight(ballsTest) == expectedListBalls)
        }

        test("Test with an empty list") {
            val emptyBalls = Map[String, (Int, Double)]()
            assert(Exercises.sortByHeavyweight(emptyBalls) == Seq())
        }
    }
}
