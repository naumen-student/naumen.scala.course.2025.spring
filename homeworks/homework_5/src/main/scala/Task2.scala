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

  case class DegreeAngle(angle: Double)
  object DegreeAngle {
    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle] {
      override def empty: DegreeAngle = DegreeAngle(0)

      override def combine(a: DegreeAngle, b: DegreeAngle): DegreeAngle =
        DegreeAngle((a.angle + b.angle) % 360)
    }
  }

  case class SquareMatrix[A: Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))
  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]] {
      override def empty: SquareMatrix[A] = {
        val zero = Monoid[A].empty
        SquareMatrix(((zero, zero, zero), (zero, zero, zero), (zero, zero, zero)))
      }

      override def combine(a: SquareMatrix[A], b: SquareMatrix[A]): SquareMatrix[A] = {
        def addTriple(t1: (A, A, A), t2: (A, A, A)): (A, A, A) =
          (
            t1._1 |+| t2._1,
            t1._2 |+| t2._2,
            t1._3 |+| t2._3
          )

        SquareMatrix((
          addTriple(a.values._1, b.values._1),
          addTriple(a.values._2, b.values._2),
          addTriple(a.values._3, b.values._3)
        ))
      }
    }
  }
}