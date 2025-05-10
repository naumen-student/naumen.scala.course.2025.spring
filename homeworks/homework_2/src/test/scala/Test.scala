import utest._

object Test extends TestSuite {

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 10) == 33) // 3 + 5 + 6 + 9 + 10 = 33
            assert(Exercises.sumOfDivBy3Or5(10, 20) == 75) // 10 + 12 + 15 + 18 + 20 = 75
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
            assert(Exercises.primeFactor(17) == Seq(17)) // Простое число
        }

        'test_sumScalars - {
            val vec0 = Exercises.Vector2D(1, 2)
            val vec1 = Exercises.Vector2D(3, 4)
            val vec2 = Exercises.Vector2D(5, 6)
            val vec3 = Exercises.Vector2D(7, 8)
            assert(Exercises.sumScalars(vec0, vec1, vec2, vec3) == 94) // 11 + 83 = 94
        }

        'test_sumCosines - {
            val vec0 = Exercises.Vector2D(1, 0)
            val vec1 = Exercises.Vector2D(0, 1)
            val vec2 = Exercises.Vector2D(1, 1)
            val vec3 = Exercises.Vector2D(-1, 1)
            assert(Exercises.sumCosines(vec0, vec1, vec2, vec3) == 0.0) // 0 + 0 = 0
        }

        'test_sortByHeavyweight - {
            val sorted = Exercises.sortByHeavyweight()
            assert(sorted.head == "Tin") // Самый легкий
            assert(sorted.last == "Graphite") // Самый тяжелый
        }
    }
}
