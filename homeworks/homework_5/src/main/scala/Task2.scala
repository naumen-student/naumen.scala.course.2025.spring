import cats._
import cats.implicits._

/*
  Задание №2
  Всё просто, для каждого кейс класса необходимо описать логику его сложения.
  Радиус-вектор должен складываться, как и любой другой вектор.
  GradeAngle всегда выражает угол [0, 360).
  SquareMatrix просто сложение квадратных матриц
 */
object Task2 extends App {
  case class RadiusVector(x: Int, y: Int)
  object RadiusVector {
    implicit val monoid: Monoid[RadiusVector] = new Monoid[RadiusVector] {
      override def empty: RadiusVector = RadiusVector(0, 0)

      override def combine(a: RadiusVector, b: RadiusVector): RadiusVector =
        RadiusVector(a.x + b.x, a.y + b.y)
    }
  }
  case class DegreeAngle(angel: Double)
  object DegreeAngle {
    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle] {
      override def empty: DegreeAngle = DegreeAngle(0.0)

      override def combine(a: DegreeAngle, b: DegreeAngle): DegreeAngle =
        DegreeAngle((a.angel + b.angel) % 360)
      }
    }

  case class SquareMatrix[A : Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))
  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]] {
      override def empty: SquareMatrix[A] = SquareMatrix(Monoid[((A, A, A), (A, A, A), (A, A, A))].empty)

      override def combine(a: SquareMatrix[A], b: SquareMatrix[A]): SquareMatrix[A] = {
        val ((a1, a2, a3), (a4, a5, a6), (a7, a8, a9)) = a.values
        val ((b1, b2, b3), (b4, b5, b6), (b7, b8, b9)) = b.values

        SquareMatrix(
          (Monoid[A].combine(a1, b1), Monoid[A].combine(a2, b2), Monoid[A].combine(a3, b3)),
          (Monoid[A].combine(a4, b4), Monoid[A].combine(a5, b5), Monoid[A].combine(a6, b6)),
          (Monoid[A].combine(a7, b7), Monoid[A].combine(a8, b8), Monoid[A].combine(a9, b9))
        )
      }
    }
  }

  val radiusVectors = Vector(RadiusVector(0, 0), RadiusVector(0, 1), RadiusVector(-1, 1))
  Monoid[RadiusVector].combineAll(radiusVectors) // RadiusVector(-1, 2)

  val gradeAngles = Vector(DegreeAngle(380), DegreeAngle(60), DegreeAngle(30))
  Monoid[DegreeAngle].combineAll(gradeAngles) // GradeAngle(90)

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
  Monoid[SquareMatrix[Int]].combineAll(matrixes)
  //  [0, 0, 0]
  //  |1, 1, 1|
  //  [0, 0, 0]
}
