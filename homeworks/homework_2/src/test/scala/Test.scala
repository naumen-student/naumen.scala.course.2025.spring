import utest._

object Test extends TestSuite{

    val tests = Tests{
        "test_divBy3Or7" - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        "sumOfDivBy3Or5" - {
            assert(Exercises.sumOfDivBy3Or5(1, 20) == 98)
            assert(Exercises.sumOfDivBy3Or5(51, 60) == 277)
            assert(Exercises.sumOfDivBy3Or5(1, 2) == 0)
        }

        "primeFactors" - {
            assert(Exercises.primeFactors(36) == Seq(2, 3))
            assert(Exercises.primeFactors(80) == Seq(2, 5))
            assert(Exercises.primeFactors(98) == Seq(2, 7))
        }

        "test_sumScalars" - {
            import Exercises.Vector2D
            val v1 = Vector2D(1, 0)
            val v2 = Vector2D(0, 1)
            val v3 = Vector2D(2, 2)
            val v4 = Vector2D(3, 4)

            assert(Exercises.sumScalars(v1, v4, v1, v4) == 6.0)
            assert(Exercises.sumScalars(v4, v2, v4, v2) == 8.0)
            assert(Exercises.sumScalars(v3, v4, v3, v4) == 28.0)
        }

        "sumCosines" - {
            import Exercises.Vector2D
            val v1 = Vector2D(1, 0)
            val v2 = Vector2D(0, 1)
            val v3 = Vector2D(-1, 0)
            val v4 = Vector2D(0, -1)

            assert(Exercises.sumCosines(v1, v2, v1, v2) == 0.0)
            assert(Exercises.sumCosines(v3, v4, v3, v4) == -0.0)
            assert(Exercises.sumCosines(v3, v1, v3, v1) == -2.0)
        }

        "sortByHeavyweight" - {
            val test1 = Map("Aluminum" -> (3,   2.6889),
                            "Tungsten" ->  (2,   19.35),
                            "Graphite" ->  (12,  2.1))
            assert(Exercises.sortByHeavyweight(test1) == Seq("Aluminum", "Tungsten", "Graphite"))

            val test2 = Map("Gold" -> (2,   19.32),
                            "Potassium" -> (14,  0.862),
                            "Calcium" ->   (8,   1.55),
                            "Cobalt" ->    (4,   8.90),
                            "Lithium" ->  (12,  0.534),
                            "Magnesium" -> (10,  1.738),
                            "Copper" ->    (3,   8.96))
            assert(Exercises.sortByHeavyweight(test2) == Seq("Gold", "Copper", "Cobalt", "Calcium", "Lithium", "Magnesium", "Potassium"))
        }
    }
}
