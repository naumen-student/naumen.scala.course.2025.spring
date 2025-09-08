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
    implicit val monoid: Monoid[RadiusVector] = new Monoid[RadiusVector]{
      override def empty: RadiusVector =  RadiusVector(0,0)

      override def combine(vector1: RadiusVector, vector2: RadiusVector): RadiusVector = {
        RadiusVector(vector1.x + vector2.x, vector1.y + vector2.y)
      }
    }
  }
  case class DegreeAngle(angel: Double)
  object DegreeAngle {
    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle] {
      override def empty: DegreeAngle = DegreeAngle(0)

      override def combine(angle1: DegreeAngle, angle2: DegreeAngle): DegreeAngle = {
        DegreeAngle((angle1.angel + angle2.angel) % 360)
      }
    }
  }

  case class SquareMatrix[A : Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))
  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]]{
      override def empty: SquareMatrix[A] = SquareMatrix(
        (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty),
        (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty),
        (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty)
      )
      override def combine(matrix1: SquareMatrix[A], matrix2: SquareMatrix[A]): SquareMatrix[A] = {
        SquareMatrix[A](matrix1.values.combine(matrix2.values))
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
