import utest._

import scala.util.Random

object Test3 extends TestSuite {
  override def tests: Tests = Tests {
    import Task3._

    'wordsMustBeCounted - (1 to 5).foreach { _ =>
      val words = Vector.fill(100)(java.util.UUID.randomUUID().toString.replace("-", ""))
      val counts = Vector.fill(100)(Random.nextInt(200)).map(_ + 1) // исключаем 0
      val expected = WordsCount(words.zip(counts).map { case (w, c) => Count(w, c) })

      val allWords = words.zip(counts).flatMap { case (word, count) => Vector.fill(count)(word) }
      val lines = Random.shuffle(allWords).grouped(50).map(_.mkString(" ")).toVector
      val result = countWords(lines)

      expected.count.foreach(x => assert(result.count.contains(x)))
    }
  }
}
