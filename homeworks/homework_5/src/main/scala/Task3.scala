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
        // Объединяем два набора счетчиков, суммируя счетчики для одинаковых слов
        val wordCounts = (x.count ++ y.count)
          .groupBy(_.word)
          .map { case (word, counts) => 
            Count(word, counts.map(_.count).sum)
          }
          .toSeq
          
        WordsCount(wordCounts)
      }
    }
  }

  def countWords(lines: Vector[String]): WordsCount = {
    // Разбиваем строки на слова, подсчитываем каждое слово и объединяем результаты
    val result = Await.result(
      mapReduce(lines) { line =>
        val words = line.split(" ").filter(_.nonEmpty)
        val counts = words
          .groupBy(identity)
          .map { case (word, occurrences) => 
            Count(word, occurrences.length)
          }
          .toSeq
        
        WordsCount(counts)
      },
      5.seconds
    )
    
    result
  }
}
