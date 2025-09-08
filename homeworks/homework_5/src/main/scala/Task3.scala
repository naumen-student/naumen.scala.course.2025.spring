  case class Count(word: String, count: Int)
  case class WordsCount(count: Seq[Count])
  object WordsCount {
    implicit val monoid: Monoid[WordsCount] = ???
    implicit val monoid: Monoid[WordsCount] = new Monoid[WordsCount] {
      override def empty: WordsCount = WordsCount(Seq.empty)

      override def combine(x: WordsCount, y: WordsCount): WordsCount =
        WordsCount(x.count ++ y.count)
    }
  }

  def countWords(lines: Vector[String]): WordsCount = ???
  def countWords(lines: Vector[String]): WordsCount = {
    val words = lines.flatMap(_.split(" "))
    val counts = words
      .groupBy(identity)
      .mapValues(_.size)
      .toSeq
      .map { case (word, count) => Count(word, count) }
    WordsCount(counts)
  }
}
