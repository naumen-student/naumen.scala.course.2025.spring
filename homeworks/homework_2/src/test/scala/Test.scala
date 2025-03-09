import utest._
import scala.math.abs

object Test extends TestSuite {

  val tests = Tests {

    "test_divBy3Or7" - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(
        0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45,
        48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90,
        91, 93, 96, 98, 99
      ))
    }

    "test_sumOfDivBy3Or5" - {
      // Для диапазона 1 до 10: 3, 5, 6, 9, 10 => сумма = 33
      assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
      // Если диапазон не содержит чисел, делящихся на 3 или 5
      assert(Exercises.sumOfDivBy3Or5(2, 2) == 0)
      // Тест с отрицательными числами:
      // Диапазон -5 до 5: -5, -3, 0, 3, 5 => сумма = 0
      assert(Exercises.sumOfDivBy3Or5(-5, 5) == 0)
    }

    "test_primeFactor" - {
      // Для чисел меньше 2 возвращается пустая последовательность
      assert(Exercises.primeFactor(1).isEmpty)
      // 80 = 2^4 * 5, ожидаем Seq(2, 5)
      assert(Exercises.primeFactor(80) == Seq(2, 5))
      // 98 = 2 * 7^2, ожидаем Seq(2, 7)
      assert(Exercises.primeFactor(98) == Seq(2, 7))
      // 36 = 2^2 * 3^2, ожидаем Seq(2, 3)
      assert(Exercises.primeFactor(36) == Seq(2, 3))
      // Простое число 17, ожидаем Seq(17)
      assert(Exercises.primeFactor(17) == Seq(17))
    }

    "test_sumScalars" - {
      // Векторы: (1,2) и (3,4): скалярное произведение = 1*3 + 2*4 = 11
      // Векторы: (5,6) и (7,8): скалярное произведение = 5*7 + 6*8 = 83
      // Общая сумма = 11 + 83 = 94
      val leftVec0  = Exercises.Vector2D(1, 2)
      val leftVec1  = Exercises.Vector2D(3, 4)
      val rightVec0 = Exercises.Vector2D(5, 6)
      val rightVec1 = Exercises.Vector2D(7, 8)
      assert(Exercises.sumScalars(leftVec0, leftVec1, rightVec0, rightVec1) == 94.0)
    }

    "test_sumCosines" - {
      // Векторы: (1,0) и (1,0): cos = 1
      // Векторы: (0,1) и (1,0): cos = 0
      // Общая сумма = 1 + 0 = 1
      val leftVec0  = Exercises.Vector2D(1, 0)
      val leftVec1  = Exercises.Vector2D(1, 0)
      val rightVec0 = Exercises.Vector2D(0, 1)
      val rightVec1 = Exercises.Vector2D(1, 0)
      val result = Exercises.sumCosines(leftVec0, leftVec1, rightVec0, rightVec1)
      assert(abs(result - 1.0) < 1e-6)
    }

    "test_sortByHeavyweight" - {
      // Тестовая коллекция шариков:
      // "A": (1, 1.0)   -> масса ~ 4.18879
      // "B": (2, 1.0)   -> масса ~ 33.5103
      // "C": (1, 2.0)   -> масса ~ 8.37758
      // Ожидаемый порядок по возрастанию массы: A, C, B
      val testBalls = Map(
        "A" -> (1, 1.0),
        "B" -> (2, 1.0),
        "C" -> (1, 2.0)
      )
      assert(Exercises.sortByHeavyweight(testBalls) == Seq("A", "C", "B"))
    }

    "test_sortByHeavyweightDefault" - {
      // Ожидаемый порядок имен для коллекции по умолчанию, вычисляемый по массе шариков:
      val expectedOrder = Seq(
        "Tin",         // (1, 7.29)
        "Platinum",    // (1, 21.45)
        "Nickel",      // (2, 8.91)
        "Aluminum",    // (3, 2.6889)
        "Titanium",    // (2, 10.50)
        "Lead",        // (2, 11.336)
        "Sodium",      // (5, 0.971)
        "Uranium",     // (2, 19.04)
        "Gold",        // (2, 19.32)
        "Tungsten",    // (2, 19.35)
        "Zirconium",   // (3, 6.45)
        "Chrome",      // (3, 7.18)
        "Iron",        // (3, 7.874)
        "Copper",      // (3, 8.96)
        "Silver",      // (4, 4.505)
        "Plutonium",   // (3, 19.25)
        "Cobalt",      // (4, 8.90)
        "Cesium",      // (7, 1.873)
        "Calcium",     // (8, 1.55)
        "Lithium",     // (12, 0.534)
        "Magnesium",   // (10, 1.738)
        "Potassium",   // (14, 0.862)
        "Graphite"     // (12, 2.1)
      )
      assert(Exercises.sortByHeavyweightDefault == expectedOrder)
    }
  }
}
