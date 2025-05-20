import cats._
import cats.implicits._

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
    private def normalize(angle: Double): Double = {
      val mod = angle % 360
      if (mod < 0) mod + 360 else mod
    }

    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle] {
      override def empty: DegreeAngle = DegreeAngle(0.0)
      override def combine(a: DegreeAngle, b: DegreeAngle): DegreeAngle = {
        val sum = a.angel + b.angel
        DegreeAngle(normalize(sum))
      }
    }
  }

  case class SquareMatrix[A: Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))
  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]] {
      private def add(a: A, b: A): A = Monoid[A].combine(a, b)

      override def empty: SquareMatrix[A] = {
        val z = Monoid[A].empty
        SquareMatrix(((z, z, z), (z, z, z), (z, z, z)))
      }

      override def combine(m1: SquareMatrix[A], m2: SquareMatrix[A]): SquareMatrix[A] = {
        val ((a11, a12, a13), (a21, a22, a23), (a31, a32, a33)) = m1.values
        val ((b11, b12, b13), (b21, b22, b23), (b31, b32, b33)) = m2.values

        SquareMatrix((
          (add(a11, b11), add(a12, b12), add(a13, b13)),
          (add(a21, b21), add(a22, b22), add(a23, b23)),
          (add(a31, b31), add(a32, b32), add(a33, b33))
        ))
      }
    }
  }

  val radiusVectors = Vector(RadiusVector(0, 0), RadiusVector(0, 1), RadiusVector(-1, 1))
  println(Monoid[RadiusVector].combineAll(radiusVectors))

  val gradeAngles = Vector(DegreeAngle(380), DegreeAngle(60), DegreeAngle(30))
  println(Monoid[DegreeAngle].combineAll(gradeAngles))

  val matrices = Vector(
    SquareMatrix(((1, 2, 3), (4, 5, 6), (7, 8, 9))),
    SquareMatrix(((-1, -2, -3), (-3, -4, -5), (-7, -8, -9)))
  )
  println(Monoid[SquareMatrix[Int]].combineAll(matrices))
}