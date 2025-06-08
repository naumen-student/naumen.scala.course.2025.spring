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
@@ -25,8 +19,25 @@ object Task3 extends App {
  case class Count(word: String, count: Int)
  case class WordsCount(count: Seq[Count])
  object WordsCount {
    implicit val monoid: Monoid[WordsCount] = ???
    implicit val monoid: Monoid[WordsCount] = new Monoid[WordsCount]{
      override def empty: WordsCount = WordsCount(Seq.empty)
      override def combine(a: WordsCount, b: WordsCount): WordsCount =
        WordsCount(
          (a.count ++ b.count)
            .groupBy(c => c.word)
            .map(wordToCount => Count(wordToCount._1, wordToCount._2.map(item => item.count).sum))
            .toSeq)
    }
  }

  def countWords(lines: Vector[String]): WordsCount = ???
  def countWords(lines: Vector[String]): WordsCount = {
    Await.result(
      mapReduce(lines)(
        s => WordsCount(
          s.split(" ").groupBy(word => word).map(group => Count(group._1, group._2.length)).toSeq
        )
      ),
      10.second
    )
  }
}
