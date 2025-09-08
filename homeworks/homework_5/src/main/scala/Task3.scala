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
        val countsMap = scala.collection.mutable.HashMap.empty[String, Int]

        def addCounts(counts: Seq[Count]): Unit =
          counts.foreach(c => countsMap.update(c.word, countsMap.getOrElse(c.word, 0) + c.count))

        addCounts(a.count)
        addCounts(b.count)

        WordsCount(countsMap.map { case (word, cnt) => Count(word, cnt) }.toSeq)
      }
    }
  }

  def countWords(lines: Vector[String]): WordsCount = {
    Await.result(
      mapReduce(lines) { line =>
        val words = line.split("\\s+").filter(_.nonEmpty)
        WordsCount(words.map(word => Count(word, 1)))
      },
      1.minute
    )
  }
}
