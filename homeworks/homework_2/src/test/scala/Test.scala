import utest._
import Exercises.Vector2D

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(0,5) == 8)
            assert(Exercises.sumOfDivBy3Or5(3,6) == 14)
            assert(Exercises.sumOfDivBy3Or5(10,20) == 75)
            assert(Exercises.sumOfDivBy3Or5(25,26) == 25)
            assert(Exercises.sumOfDivBy3Or5(28,29) == 0)
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(10) == Seq(2, 5))
            assert(Exercises.primeFactor(5) == Seq(5))
            assert(Exercises.primeFactor(49) == Seq(7, 7))
            assert(Exercises.primeFactor(50) == Seq(2, 5, 5))
            assert(Exercises.primeFactor(96) == Seq(2, 2, 2, 2, 2, 3))
        }

        'test_sumScalars - {
            assert(Exercises.sumScalars(Vector2D(0,0),Vector2D(0,0), Vector2D(0,0), Vector2D(0,0)) == 0)
            assert(Exercises.sumScalars(Vector2D(1,1),Vector2D(-1,-1), Vector2D(1,1), Vector2D(0,0)) == -2)
            assert(Exercises.sumScalars(Vector2D(2,4),Vector2D(8,2), Vector2D(1,10), Vector2D(30,2)) == 74)
        }

        'test_sumCosines - {
            assert(Exercises.sumCosines(Vector2D(1, 1), Vector2D(-1, -1), Vector2D(1, 1), Vector2D(1, 1)) == 0)
            assert(Exercises.sumCosines(Vector2D(2, 4), Vector2D(8, 2), Vector2D(1, 10), Vector2D(30, 2)) < 0.8162 + 0.001 &&
              Exercises.sumCosines(Vector2D(2, 4), Vector2D(8, 2), Vector2D(1, 10), Vector2D(30, 2)) > 0.8162 - 0.001)
        }

        'test_sortByHeavyweight - {
            assert(Exercises.sortByHeavyweight(Map(
                "Aluminum" -> (1, 2.6889), "Tungsten" -> (2, 19.35), "Graphite" -> (3, 2.1)))
              == Seq("Aluminum", "Graphite", "Tungsten"))

            assert(Exercises.sortByHeavyweight(Map(
                "Aluminum" -> (10, 2.6889), "Tungsten" -> (10, 19.35), "Graphite" -> (10, 2.1)))
              == Seq( "Graphite", "Aluminum", "Tungsten"))
        }
    }
}
