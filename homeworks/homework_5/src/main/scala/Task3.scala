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
      override def empty: WordsCount = WordsCount(Seq.empty)

      override def combine(x: WordsCount, y: WordsCount): WordsCount = {
        val combinedMap = (x.count ++ y.count)
          .groupBy(_.word)
          .view
          .mapValues(_.map(_.count).sum)
          .map { case (word, total) => Count(word, total) }
          .toSeq
        WordsCount(combinedMap)
      }
    }
  }

  def countWords(lines: Vector[String]): WordsCount = {
    val futureResult: Future[WordsCount] = mapReduce(lines) { line =>
      val words = line.split("\\s+").filter(_.nonEmpty)
      val counts = words.groupBy(identity).view.mapValues(_.length).toMap
      val countSeq = counts.map { case (word, cnt) => Count(word, cnt) }.toSeq
      WordsCount(countSeq)
    }

    Await.result(futureResult, 5.seconds)
  }
}
