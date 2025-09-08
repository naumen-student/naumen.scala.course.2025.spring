import utest._

object Test extends TestSuite{

    val tests = Tests{
        test("test_divBy3Or7") {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        test("test_sumOfDivBy3Or5") {
            assert(Exercises.sumOfDivBy3Or5(0, 2) == 0L)
            assert(Exercises.sumOfDivBy3Or5(4, 30) == 222L)
            assert(Exercises.sumOfDivBy3Or5(36, 45) == 202L )
        }

        test("test_primeFactor") {
            assert(Exercises.primeFactor(0) == Seq.empty)
            assert(Exercises.primeFactor(1) == Seq.empty)
            assert(Exercises.primeFactor(35) == Seq(5,7))
            assert(Exercises.primeFactor(80) == Seq(2,5))
            assert(Exercises.primeFactor(98) == Seq(2,7))
        }

        test("test_sumScalars") {
            val leftVec0 = Exercises.Vector2D(4,5)
            val leftVec1 = Exercises.Vector2D(10,20)
            val rightVec0 = Exercises.Vector2D(1,6)
            val rightVec1 = Exercises.Vector2D(7,4)
            val expectedSumScalars = Exercises.scalar(leftVec0, leftVec1) + Exercises.scalar(rightVec0, rightVec1)

            assert(Exercises.sumScalars(leftVec0, leftVec1, rightVec0, rightVec1) == expectedSumScalars)
        }

        test("test_sumCosines") {
            val leftVec0 = Exercises.Vector2D(4,5)
            val leftVec1 = Exercises.Vector2D(10,20)
            val rightVec0 = Exercises.Vector2D(1,6)
            val rightVec1 = Exercises.Vector2D(7,4)
            val expectedSumCosines = Exercises.cosBetween(leftVec0, leftVec1) + Exercises.cosBetween(rightVec0, rightVec1)

            assert(Exercises.sumCosines(leftVec0, leftVec1, rightVec0, rightVec1) == expectedSumCosines)
        }

        test("test_sortByHeavyweight") {
            val ballsTest: Map[String, (Int, Double)] =
                Map(
                    "Aluminum" -> (3,   2.6889), "Graphite" ->  (12,  2.1),   "Iron" ->      (3,   7.874),
                    "Magnesium" -> (10,  1.738), "Titanium" ->  (2,   10.50), "Silver" ->    (4,   4.505),
                    "Chrome" ->   (3,   7.18)
                )

            val expectedListBalls = Seq("Aluminum", "Titanium", "Chrome", "Iron", "Silver", "Magnesium", "Graphite" )
            assert(Exercises.sortByHeavyweight(ballsTest) == expectedListBalls)
        }

        test("test_sortByHeavyweight_Equals") {
            val ballsTest: Map[String, (Int, Double)] =
                Map(
                    "Platinum" ->  (1,   21.45), "Chrome" ->   (3,   7.15)
                )

            val expectedListBalls = Seq("Platinum", "Chrome")
            assert(Exercises.sortByHeavyweight(ballsTest) == expectedListBalls)
        }
    }
}
