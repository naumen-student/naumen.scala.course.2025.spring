import cats.Monoid
import cats.implicits._

object Task2 extends App {
  case class RadiusVector(x: Int, y: Int)
  object RadiusVector {
    implicit val monoid: Monoid[RadiusVector] = new Monoid[RadiusVector] {
      def empty: RadiusVector = RadiusVector(0, 0)

      def combine(x: RadiusVector, y: RadiusVector): RadiusVector =
        RadiusVector(x.x + y.x, x.y + y.y)
    }
  }
  case class DegreeAngle(angel: Double)
  object DegreeAngle {
    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle] {
      def empty: DegreeAngle = DegreeAngle(0)

      def combine(x: DegreeAngle, y: DegreeAngle): DegreeAngle = {
        val rawSum = x.angel + y.angel
        val normalizedSum = if (rawSum < 0) (rawSum % 360 + 360) % 360 else rawSum % 360
        DegreeAngle(normalizedSum)
      }
    }
  }
  case class SquareMatrix[A: Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))
  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]] {
      def empty: SquareMatrix[A] =
        SquareMatrix(
          (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty),
          (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty),
          (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty)
        )
      def combine(x: SquareMatrix[A], y: SquareMatrix[A]): SquareMatrix[A] =
        SquareMatrix(
          (
            Monoid[A].combine(x.values._1._1, y.values._1._1),
            Monoid[A].combine(x.values._1._2, y.values._1._2),
            Monoid[A].combine(x.values._1._3, y.values._1._3)
          ),
          (
            Monoid[A].combine(x.values._2._1, y.values._2._1),
            Monoid[A].combine(x.values._2._2, y.values._2._2),
            Monoid[A].combine(x.values._2._3, y.values._2._3)
          ),
          (
            Monoid[A].combine(x.values._3._1, y.values._3._1),
            Monoid[A].combine(x.values._3._2, y.values._3._2),
            Monoid[A].combine(x.values._3._3, y.values._3._3)
          )
        )
    }
  }
  val radiusVectors = Vector(RadiusVector(0, 0), RadiusVector(0, 1), RadiusVector(-1, 1))
  val combinedRadiusVector = Monoid[RadiusVector].combineAll(radiusVectors) // RadiusVector(-1, 2)

  val gradeAngles = Vector(DegreeAngle(380), DegreeAngle(60), DegreeAngle(30))
  val combinedGradeAngle = Monoid[DegreeAngle].combineAll(gradeAngles) // DegreeAngle(90)

  val matrixes = Vector(
    SquareMatrix(
      (
        (1, 2, 3),
        (4, 5, 6),
        (7, 8, 9)
      )
    ),
    SquareMatrix(
      (
        (-1, -2, -3),
        (-3, -4, -5),
        (-7, -8, -9)
      )
    )
  )
  val combinedMatrix = Monoid[SquareMatrix[Int]].combineAll(matrixes)
}
