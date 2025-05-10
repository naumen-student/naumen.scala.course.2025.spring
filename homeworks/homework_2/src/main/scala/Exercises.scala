    /*Реализовать функцию, которая возвращает сумму всех целых чисел в заданном диапазоне (от iForm до iTo), которые делятся
    на 3 или на 5.*/
    /*Реализовать юнит-тесты в src/test/scala для данной функции.*/
    def sumOfDivBy3Or5(iFrom: Int, iTo: Int): Long = ???
    def sumOfDivBy3Or5(iFrom: Int, iTo: Int): Long = {
        (iFrom to iTo)
          .filter(i => i % 3 == 0 || i % 5 == 0)
          .map(_.toLong)
          .sum
    }



@@ -25,7 +30,24 @@ object Exercises {
    Число 80 раскладывается на множители 1 * 2 * 2 * 2 * 2 * 5, результат выполнения функции => Seq(2, 5).
    Число 98 можно разложить на множители 1 * 2 * 7 * 7, результат выполнения функции => Seq(2, 7).*/
    /*Реализовать юнит-тесты в src/test/scala для данной функции.*/
    def primeFactor(number: Int): Seq[Int] = ???
    import scala.collection.mutable.ListBuffer

    def primeFactor(number: Int): Seq[Int] = {
        if (number < 2) return Seq.empty
        val factors = ListBuffer[Int]()
        var n = number
        var i = 2
        while (i * i <= n) {
            if (n % i == 0) {
                if (!factors.contains(i)) factors += i
                n /= i
            } else {
                i += 1
            }
        }
        if (n > 1 && !factors.contains(n)) factors += n
        factors.toSeq
    }



@@ -40,15 +62,15 @@ object Exercises {
    def abs(vec: Vector2D): Double = java.lang.Math.sqrt(vec.x * vec.x + vec.y * vec.y)
    def scalar(vec0: Vector2D, vec1: Vector2D): Double = vec0.x * vec1.x + vec0.y * vec1.y
    def cosBetween(vec0: Vector2D, vec1: Vector2D): Double = scalar(vec0, vec1) / abs(vec0) / abs(vec1)
    //def sumByFunc(leftVec0: Vector2D, leftVec1: Vector2D, ???, rightVec0: Vector2D, rightVec1: Vector2D) = ???
    /*
    def sumByFunc(leftVec0: Vector2D, leftVec1: Vector2D, func: (Vector2D, Vector2D) => Double, rightVec0: Vector2D, rightVec1: Vector2D): Double = {
        func(leftVec0, leftVec1) + func(rightVec0, rightVec1)
    }

    def sumScalars(leftVec0: Vector2D, leftVec1: Vector2D, rightVec0: Vector2D, rightVec1: Vector2D): Double =
        sumByFunc(leftVec0, leftVec1, scalar, rightVec0, rightVec1)
    */
    /*

    def sumCosines(leftVec0: Vector2D, leftVec1: Vector2D, rightVec0: Vector2D, rightVec1: Vector2D): Double =
        sumByFunc(leftVec0, leftVec1, cosBetween, rightVec0, rightVec1)
    */



@@ -71,6 +93,12 @@ object Exercises {
            "Chrome" ->   (3,   7.18),   "Cesium" ->    (7,   1.873), "Zirconium" -> (3,   6.45)
        )

    def sortByHeavyweight(ballsArray: Map[String, (Int, Double)] = balls): Seq[String] = ???
    def sortByHeavyweight(ballsArray: Map[String, (Int, Double)] = balls): Seq[String] = {
        ballsArray.toSeq.map { case (name, (radius, density)) =>
            val volume = (4.0 / 3) * Math.PI * Math.pow(radius, 3)
            val mass = volume * density
            (name, mass)
        }.sortBy(_._2).map(_._1)
    }

}
