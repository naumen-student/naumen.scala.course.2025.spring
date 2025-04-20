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

  case class SquareMatrix[A: Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))
  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]] {
      override def empty: SquareMatrix[A] = {
        val z = Monoid[A].empty
        SquareMatrix(((z, z, z), (z, z, z), (z, z, z)))
      }

      override def combine(a: SquareMatrix[A], b: SquareMatrix[A]): SquareMatrix[A] = {
        def combineRow(r1: (A, A, A), r2: (A, A, A)): (A, A, A) = (
          r1._1 |+| r2._1,
          r1._2 |+| r2._2,
          r1._3 |+| r2._3
        )

        val ((a11, a12, a13), (a21, a22, a23), (a31, a32, a33)) = a.values
        val ((b11, b12, b13), (b21, b22, b23), (b31, b32, b33)) = b.values

        SquareMatrix(
          (
            combineRow((a11, a12, a13), (b11, b12, b13)),
            combineRow((a21, a22, a23), (b21, b22, b23)),
            combineRow((a31, a32, a33), (b31, b32, b33))
          )
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
