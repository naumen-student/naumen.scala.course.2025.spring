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
      def empty: RadiusVector = RadiusVector(0, 0)
      def combine(a: RadiusVector, b: RadiusVector): RadiusVector =
        RadiusVector(a.x + b.x, a.y + b.y)
    }
  }

  case class DegreeAngle(angle: Double){
    val angel : Double = (360 * ((angle.abs / 360).toInt + 1) + angle) % 360
  }
  object DegreeAngle {
    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle]{
      override def empty: DegreeAngle = DegreeAngle(0)

      override def combine(x: DegreeAngle, y: DegreeAngle): DegreeAngle = {
        DegreeAngle((x.angel + y.angel) % 360)
      }
    }
  }

  case class SquareMatrix[A: Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))
  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]] {
      def empty: SquareMatrix[A] = SquareMatrix(
        (
          (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty),
          (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty),
          (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty)
        )
      )

      def combine(a: SquareMatrix[A], b: SquareMatrix[A]): SquareMatrix[A] = {
        def combineTuples(t1: (A, A, A), t2: (A, A, A)): (A, A, A) =
          (t1._1 |+| t2._1, t1._2 |+| t2._2, t1._3 |+| t2._3)

        SquareMatrix(
          (
            combineTuples(a.values._1, b.values._1),
            combineTuples(a.values._2, b.values._2),
            combineTuples(a.values._3, b.values._3)
          )
        )
      }
    }
  }

  // Примеры использования
  val radiusVectors = Vector(RadiusVector(0, 0), RadiusVector(0, 1), RadiusVector(-1, 1))
  println(Monoid[RadiusVector].combineAll(radiusVectors)) // RadiusVector(-1, 2)

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
        (-4, -5, -6),
        (-7, -8, -9)
      )
    )
  )
  println(Monoid[SquareMatrix[Int]].combineAll(matrixes))
  // SquareMatrix(((0,0,0),(0,0,0),(0,0,0)))
}