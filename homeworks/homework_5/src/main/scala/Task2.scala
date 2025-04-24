import cats._
import cats.implicits._

object Task2 extends App {
  case class RadiusVector(x: Int, y: Int)
  object RadiusVector {
    implicit val monoid: Monoid[RadiusVector] = new Monoid[RadiusVector]{
      override def empty: RadiusVector = RadiusVector(0, 0)
      override def combine(vectorA: RadiusVector, vectorB: RadiusVector): RadiusVector = {
        RadiusVector(vectorA.x + vectorB.x, vectorA.y + vectorB.y)
      }
    }
  }
  case class DegreeAngle(angle: Double)
  object DegreeAngle {
    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle]{
      override def empty: DegreeAngle = DegreeAngle(0)
      override def combine(angleA: DegreeAngle, angleB: DegreeAngle): DegreeAngle = {
        DegreeAngle((angleA.angle + angleB.angle) % 360)
      }
    }
  }

  case class SquareMatrix[A : Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))
  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]]{
      override def empty: SquareMatrix[A] = SquareMatrix((Monoid[A].empty, Monoid[A].empty, Monoid[A].empty),
        (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty),
        (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty))
      override def combine(a: SquareMatrix[A], b: SquareMatrix[A]): SquareMatrix[A] = {
        SquareMatrix(a.values.combine(b.values))
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