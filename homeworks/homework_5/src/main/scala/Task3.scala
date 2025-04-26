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

      def combine(x: WordsCount, y: WordsCount): WordsCount = {
        val combinedMap = (x.count ++ y.count)
          .groupBy(_.word)
          .map { case (word, counts) =>
            Count(word, counts.map(_.count).sum)
          }.toSeq
        WordsCount(combinedMap)
      }
    }
  }

  def countWords(lines: Vector[String]): WordsCount = {
    val futureResult: Future[WordsCount] = mapReduce(lines) { line =>
      val wordCounts = line.split("\\s+").filter(_.nonEmpty)
        .groupBy(identity)
        .map { case (word, group) => Count(word, group.length) }
        .toSeq
      WordsCount(wordCounts)
    }

    import scala.concurrent.Await
    import scala.concurrent.duration._
    Await.result(futureResult, 10.seconds)
  }
}
