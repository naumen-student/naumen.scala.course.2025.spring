import utest._
import Exercises._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
    }
}
'test_sumOfDivBy3Or5 - {
    assert(sumOfDivBy3Or5(1, 10) == 33)
    assert(sumOfDivBy3Or5(10, 15) == 45)
    assert(Exercises.sumOfDivBy3Or5(0, 20) == 98)
    assert(sumOfDivBy3Or5(3, 3) == 3)
}

'test_primeFactor - {
    assert(primeFactor(80) == Seq(2, 5))
    assert(primeFactor(98) == Seq(2, 7))
    assert(primeFactor(17) == Seq(17))
}

'test_sumScalars - {
    val vec00 = Vector2D(0, 0)
    val vec11 = Vector2D(1, 1)
    val vec10 = Vector2D(1, 0)
    val vec01 = Vector2D(0, 1)
    val vec23 = Vector2D(2, 3)
    val vec45 = Vector2D(4, 5)

    assert(sumScalars(vec10, vec01, vec10, vec10) == 1.0)
    assert(sumScalars(vec11, vec11, vec23, vec45) == 35.0)
    assert(sumScalars(vec00, vec11, vec10, vec01) == 0.0)
    assert(sumScalars(vec10, vec10, vec01, vec01) == 2.0)
}

'test_sumCosines - {
    val vec00 = Vector2D(0, 0)
    val vec11 = Vector2D(1, 1)
    val vec10 = Vector2D(1, 0)
    val vec01 = Vector2D(0, 1)

    assert(sumCosines(vec10, vec10, vec01, vec01) == 2.0)
    assert(sumCosines(vec10, vec01, vec10, vec10) == 1.0)
    assert(sumCosines(vec11, vec10, vec01, vec10).toString.take(5) == "0.707")
    assert(sumCosines(vec00, vec11, vec10, vec01).isNaN)
}

'test_sortByHeavyweight - {
    val testBalls = Map(
        "Light" -> (1, 1.0),
        "Medium" -> (1, 2.0),
        "Heavy" -> (2, 1.0),
        "Heaviest" -> (2, 2.0)
    )

    assert(sortByHeavyweight(testBalls) == Seq("Light", "Medium", "Heavy", "Heaviest"))
    assert(sortByHeavyweight(Map.empty) == Seq())

    val sorted = sortByHeavyweight()
    assert(sorted.head == "Lithium" || sorted.head == "Potassium")
    assert(sorted.last == "Platinum" || sorted.last == "Tungsten")
    assert(sorted.indexOf("Graphite") < sorted.indexOf("Magnesium"))
}