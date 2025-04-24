import cats._
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

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
        val aMap = a.count.map(c => c.word -> c.count).toMap
        val bMap = b.count.map(c => c.word -> c.count).toMap
        val combined = aMap |+| bMap
        WordsCount(combined.map { case (word, cnt) => Count(word, cnt) }.toSeq)
      }
    }
  }

  def countWords(lines: Vector[String]): WordsCount = {
    val futureCount = mapReduce(lines) { line =>
      val words = line.split("\\s+").filter(_.nonEmpty)
      val counts = words.groupBy(identity).map { case (w, ws) => Count(w, ws.length) }.toSeq
      WordsCount(counts)
    }
    Await.result(futureCount, 10.seconds)
  }
}
