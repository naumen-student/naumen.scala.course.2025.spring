import cats._
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

/*
  Задание №3
  Всё просто, нужно посчитать количество строк.
  Реализуйте функцию countWords, которая принимает список строк.
  Обязательно использовать функцию mapReduce.
 */
object Task3 extends App {
  def mapReduce[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    values
      .grouped(groupSize)
      .toVector
      .traverse(group => Future(group.foldMap(func)))
      .map(_.combineAll)
  }

  case class Count(word: String, count: Int)
  case class WordsCount(count: Seq[Count])

  object WordsCount {
    implicit val monoid: Monoid[WordsCount] = new Monoid[WordsCount] {
      def empty: WordsCount = WordsCount(Seq.empty)

      def combine(a: WordsCount, b: WordsCount): WordsCount = {
        val mapA: Map[String, Int] = a.count.groupBy(_.word).map { case (word, counts) => word -> counts.map(_.count).sum }
        val mapB: Map[String, Int] = b.count.groupBy(_.word).map { case (word, counts) => word -> counts.map(_.count).sum }

        val combinedKeys = mapA.keySet ++ mapB.keySet
        val combinedCounts = combinedKeys.toSeq.map { word =>
          Count(word, mapA.getOrElse(word, 0) + mapB.getOrElse(word, 0))
        }.sortBy(_.word)

        WordsCount(combinedCounts)
      }
    }
  }

  def countWords(lines: Vector[String]): WordsCount = {
    val singleCounts: Vector[WordsCount] = lines.map { line =>
      val words = line.split("\\s+").filter(_.nonEmpty)
      val counts = words.groupBy(identity).map { case (word, occurrences) => Count(word, occurrences.length) }.toSeq.sortBy(_.word)
      WordsCount(counts)
    }

    Await.result(mapReduce(singleCounts)(identity), 10.seconds)
  }
}
