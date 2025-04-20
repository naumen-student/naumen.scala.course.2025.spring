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
      override def combine(x: WordsCount, y: WordsCount): WordsCount = {
        val counts = x.count ++ y.count

        WordsCount(counts
          .groupBy(_.word)
          .map {
            case (word, counts) => Count(word, counts.map(_.count).sum)
          }.toSeq)
      }

      override def empty: WordsCount = WordsCount(Seq.empty)
    }
  }

  def countWords(lines: Vector[String]): WordsCount = {
      val wordsCountFuture = mapReduce(lines)(
        line => {
          val words = line.split(" ")
          WordsCount(words.map(word => Count(word, 1)))
        }
      )

      Await.result(wordsCountFuture, 5.seconds)
  }
}