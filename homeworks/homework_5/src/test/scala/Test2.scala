import cats.implicits._
import utest._

import scala.util.Random

object Test2 extends TestSuite {
  override def tests: Tests = Tests {
    import Task2._

    'vectorsMustSum - (1 to 5).foreach { _ =>
      val xs = Vector.fill(50)(Random.nextInt)
      val ys = Vector.fill(50)(Random.nextInt)
      val vectors = xs.zip(ys).map { case (x, y) => RadiusVector(x, y) }
      val resultVector = RadiusVector(xs.sum, ys.sum)
      assert(vectors.combineAll == resultVector)
    }
    'angleMustSum - (1 to 5).foreach { _ =>
      val angles = Vector.fill(50)(DegreeAngle(Random.nextInt))
      val result = angles.map(_.angel).sum % 360
      assert(angles.combineAll == DegreeAngle(result))
    }
    'matrixMustSum - (1 to 5).foreach { _ =>
      val firstRow = List.fill(50)(List.fill(3)(Random.nextInt))
      val secondRow = List.fill(50)(List.fill(3)(Random.nextInt))
      val thirdRow = List.fill(50)(List.fill(3)(Random.nextInt))
      val matrixs = firstRow.zip(secondRow.zip(thirdRow)).map {
        case (x1 :: y1 :: z1 :: Nil, (x2 :: y2 :: z2 :: Nil, x3 :: y3 :: z3 :: Nil)) => SquareMatrix(
          ((x1, y1, z1), (x2, y2, z2), (x3, y3, z3))
        )
      }
      def countRow(row: List[List[Int]]): (Int, Int, Int) =
        row.foldLeft((0, 0, 0)) { (acc, next) =>
          next match {
            case x :: y :: z :: Nil => (acc._1 + x, acc._2 + y, acc._3 + z)
            case _ => throw new Exception()
          }
        }
      val result = SquareMatrix((
        countRow(firstRow),
        countRow(secondRow),
        countRow(thirdRow)
      ))
      assert(matrixs.combineAll == result)
    }
  }
}
