import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        // Задача 1
        'sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 3) == 3)
            assert(Exercises.sumOfDivBy3Or5(5, 9) == 20)
            assert(Exercises.sumOfDivBy3Or5(0, 100) == 2418)
        }

        // Задача 2
        'primeFactor - {
            assert(Exercises.primeFactor(1) == Seq())
            assert(Exercises.primeFactor(997) == Seq(997))
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(60) == Seq(2, 3, 5))
            assert(Exercises.primeFactor(2048) == Seq(2))
            assert(Exercises.primeFactor(209370) == Seq(2, 3, 5, 7, 997))
        }

        // Задача 3
        'sumScalars - {
            val v1 = Exercises.Vector2D(1, 2)
            val v2 = Exercises.Vector2D(3, 4)
            val v3 = Exercises.Vector2D(4, 1)
            val v4 = Exercises.Vector2D(5, 6)

            assert(Exercises.sumScalars(v1, v2, v3, v4) == 11+26)
            assert(Exercises.sumScalars(v4, v1, v3, v2) == 17+16)
            assert(Exercises.sumScalars(v3, v1, v4, v2) == 6+39)
        }
        'sumCosines - {
            val v1 = Exercises.Vector2D(1, 1)
            val v2 = Exercises.Vector2D(2, 2)
            val v3 = Exercises.Vector2D(0, 1)
            val v4 = Exercises.Vector2D(1, 0)

            // погрешность 10^-9
            assert(math.abs(Exercises.sumCosines(v1, v1, v1, v2) - 2) < 0.000000001) // 1 + 1
            assert(math.abs(Exercises.sumCosines(v3, v4, v2, v3) - 0.707106781) < 0.000000001) // 0 + sqrt(2)/2
        }

        // Задача 4
        'sortByHeavyweight - {
            val m1 = Map("Tin" -> (1, 7.29), "Cesium" -> (7, 1.873), "Zirconium" -> (3, 6.45))
            assert(Exercises.sortByHeavyweight(m1) == Seq("Tin", "Zirconium", "Cesium"))

            val m2 = Map("Gold" ->     (2,   19.32), "Nickel" ->   (2,   8.91), "Silver" ->    (4,   4.505))
            assert(Exercises.sortByHeavyweight(m2) == Seq("Nickel", "Gold", "Silver"))

            val m3: Map[String, (Int, Double)] = Map()
            assert(Exercises.sortByHeavyweight(m3) == Seq[String]())
        }
    }
}