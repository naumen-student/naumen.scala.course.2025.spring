import utest._

object Test1 extends TestSuite {
  val veryLittleCat = "очень маленький кот"
  val littleCat = "маленький кот"
  val normalCat = "кот"
  val bigCat = "большой кот"
  val veryBigCat = "очень большой кот"
  val inBox = "в коробке"

  override def tests: Tests = Tests {
    import Task1._
    import Task1.ShowInstance._
    import Task1.ShowSyntax._
    def check[A : Show](toShow: String => A, phrase: String, isInBox: Boolean = false): Unit = {
      (1 to 5).foreach { _ =>
        val randomName = java.util.UUID.randomUUID().toString.replace("-", "")
        assert(
          toShow(randomName).show == (if (isInBox) s"$phrase $randomName $inBox" else s"$phrase $randomName")
        )
      }
    }

    'showCats - {
      'veryLittleCat - check(VeryLittleCat, veryLittleCat)
      'littleCat - check(LittleCat, littleCat)
      'normalCat - check(NormalCat, normalCat)
      'bigCat - check(BigCat, bigCat)
      'veryBigCat - check(VeryBigCat, veryBigCat)
    }
    'showBox - {
      'emptyBox - assert(EmptyBox.show == "пустая коробка")
      'catInBox - {
        'veryLittleCat - check(name => BoxWith(VeryLittleCat(name)), veryLittleCat, true)
        'littleCat - check(name => BoxWith(LittleCat(name)), littleCat, true)
        'normalCat - check(name => BoxWith(NormalCat(name)), normalCat, true)
        'bigCat - check(name => BoxWith(BigCat(name)), bigCat, true)
        'veryBigCat - check(name => BoxWith(VeryBigCat(name)), veryBigCat, true)
      }
    }
  }
}
