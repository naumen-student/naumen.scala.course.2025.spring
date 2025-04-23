import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 10) == 33) // 3 + 5 + 6 + 9 + 10 = 33
            assert(Exercises.sumOfDivBy3Or5(1, 16) == 60) // 3 + 5 + 6 + 9 + 10 + 12 + 15 = 60
            assert(Exercises.sumOfDivBy3Or5(10, 20) == 75) // 10 + 12 + 15 + 18 + 20 = 75
            assert(Exercises.sumOfDivBy3Or5(0, 0) == 0)
        }
        
        'test_primeFactor - {
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
            assert(Exercises.primeFactor(24) == Seq(2, 3))
            assert(Exercises.primeFactor(1) == Seq())
            assert(Exercises.primeFactor(13) == Seq(13))
            assert(Exercises.primeFactor(2) == Seq(2))
        }
        
        'test_sumScalars - {
            val v1 = Exercises.Vector2D(1, 0)
            val v2 = Exercises.Vector2D(0, 1)
            val v3 = Exercises.Vector2D(2, 0)
            val v4 = Exercises.Vector2D(0, 2)
            
            assert(Exercises.sumScalars(v1, v2, v3, v4) == 0)
            
            val v5 = Exercises.Vector2D(1, 1)
            val v6 = Exercises.Vector2D(2, 2)
            val v7 = Exercises.Vector2D(3, 3)
            val v8 = Exercises.Vector2D(4, 4)
            
            assert(math.abs(Exercises.sumScalars(v5, v6, v7, v8) - 28.0) < 0.001)
        }
        
        'test_sumCosines - {
            val v1 = Exercises.Vector2D(1, 0)
            val v2 = Exercises.Vector2D(0, 1)
            val v3 = Exercises.Vector2D(1, 0)
            val v4 = Exercises.Vector2D(0, 1)
            
            assert(Exercises.sumCosines(v1, v2, v3, v4) == 0)
            
            val v5 = Exercises.Vector2D(1, 0)
            val v6 = Exercises.Vector2D(1, 0)
            val v7 = Exercises.Vector2D(0, 1)
            val v8 = Exercises.Vector2D(0, 1)
            
            assert(Exercises.sumCosines(v5, v6, v7, v8) == 2)
            
            val v9 = Exercises.Vector2D(1, 1)
            val v10 = Exercises.Vector2D(1, 1)
            
            // cos между (1,0) и (1,1) ≈ 0.7071, cos между (0,1) и (1,1) ≈ 0.7071
            // сумма ≈ 1.4142
            assert(math.abs(Exercises.sumCosines(v1, v9, v2, v10) - 1.4142) < 0.001)
        }
        
        'test_sortByHeavyweight - {
            // Check if the first and last elements are correct
            val result = Exercises.sortByHeavyweight()
            assert(result.head == "Tin") // Tin should be the lightest
            assert(result.last == "Graphite") // Graphite should be the heaviest
            
            // Test with custom input
            val customBalls = Map(
                "A" -> (1, 1.0),   // Weight = 4/3 * π * 1³ * 1 = 4π/3
                "B" -> (2, 1.0),   // Weight = 4/3 * π * 2³ * 1 = 32π/3
                "C" -> (1, 2.0)    // Weight = 4/3 * π * 1³ * 2 = 8π/3
            )
            val customResult = Exercises.sortByHeavyweight(customBalls)
            assert(customResult == Seq("A", "C", "B"))
        }
    }
}
