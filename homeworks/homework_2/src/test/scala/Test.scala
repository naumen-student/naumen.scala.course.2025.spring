import utest._
import Exercises._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        
        'test_sumOfDivBy3Or5 - {
          assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
          assert(Exercises.sumOfDivBy3Or5(13, 15) == 15)
          assert(Exercises.sumOfDivBy3Or5(0, 20) == 98)
        }
    
        'test_primeFactor - {
            assert(primeFactor(1) == Seq.empty)
            assert(primeFactor(2) == Seq(2))
            assert(primeFactor(24) == Seq(2, 3))
            assert(primeFactor(98) == Seq(2, 7))
        }
    
         'test_sumScalars - {
            assert(Exersises.sumScalars(Vector2D(20, 20), Vector2D(30, 30), Vector2D(40, 40), Vector2D(50, 50)) == 5200)
            assert(Exersises.sumCosines(Vector2D(20, 20), Vector2D(30, 30), Vector2D(40, 40), Vector2D(50, 50)) == 2)
        }
    
        'test_sortByHeavyweight - {
            assert(Exersises.sortByHeavyweight(Map("1"->(4,1.0),"2"->(2,1.0),"3"->(3,1.0)))==Seq("2","3","1"))
            assert(Exercises.sortByHeavyweight(Map("1"->(1,5.0),"2"->(1,7.0),"3"->(1,2.5)))==Seq("3","1","2"))
            assert(Exercises.sortByHeavyweight(Map("1"->(3,3.0),"2"->(1,7.0),"3"->(2,6.5),"4"->(5,0.01)))==Seq("4","2","3","1"))
            assert(Exercises.sortByHeavyweight() ==Seq(
                "Tin", "Platinum", "Aluminum", "Sodium", "Nickel", "Titanium", "Lead", "Zirconium", "Chrome", "Iron", "Silver", "Uranium", "Lithium", "Gold", "Tungsten", "Copper", "Cesium", "Calcium", "Cobalt", "Potassium", "Plutonium", "Magnesium", "Graphite"
            ))
        }
    }
}
