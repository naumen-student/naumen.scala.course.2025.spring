Original file line number	Diff line number	Diff line change
@@ -8,5 +8,48 @@ object Test extends TestSuite{
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
            assert(Exercises.sumOfDivBy3Or5(10, 1) == 0)
            assert(Exercises.sumOfDivBy3Or5(1, 100000) == 2333339333)
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(11) == Seq(11))
            assert(Exercises.primeFactor(-98) == Seq(2, 7))
        }

        'test_sumScalars - {
            val v = Vector2D(2, 3)
            assert(sumScalars(v, v, v, v) === 26.0) // (2*2 + 3*3)*2 = 13*2

            val v1 = Vector2D(1, 2)
            val v2 = Vector2D(3, 4)
            val v3 = Vector2D(5, 6)
            val v4 = Vector2D(7, 8)
            assert(sumScalars(v1, v2, v3, v4) === 94.0) // (1*3 + 2*4) + (5*7 + 6*8) = 11 + 83
        }

        'test_sumCosines - {
            val vOrtho1 = Vector2D(1, 0)
            val vOrtho2 = Vector2D(0, 1)
            val vSame = Vector2D(5, 0)
            assert(sumCosines(vOrtho1, vOrtho2, vSame, vSame) === 1.0)

            val v1 = Vector2D(1, 1)
            val v2 = Vector2D(1, 0)
            val expected = (1.0 / Math.sqrt(2)) * 2 // Две пары
            assert(math.abs(sumCosines(v1, v2, v1, v2) - expected) < 0.0001)
        }
        'test_sortByHeavyweight - {
            val testBalls = Map(
                "Light"  -> (1, 1.0),    // масса ≈4.188
                "Medium" -> (2, 1.0),    // масса ≈33.51
                "Heavy"  -> (1, 10.0)    // масса ≈41.88
            )
            assert(sortByHeavyweight(testBalls) === Seq("Light", "Medium", "Heavy"))
        }
    }
}
