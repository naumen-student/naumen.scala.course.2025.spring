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

    test("showCats") {
      test("veryLittleCat") - check(VeryLittleCat.apply, veryLittleCat) 
      test("littleCat") - check(LittleCat.apply, littleCat)
      test("normalCat") - check(NormalCat.apply, normalCat)
      test("bigCat") - check(BigCat.apply, bigCat)
      test("veryBigCat") - check(VeryBigCat.apply, veryBigCat)
    }
    test("showBox") {
      test("emptyBox") - assert(EmptyBox.show == "пустая коробка")
      test("catInBox") - {
        test("veryLittleCat") - check(name => BoxWith(VeryLittleCat(name)), veryLittleCat, true)
        test("littleCat") - check(name => BoxWith(LittleCat(name)), littleCat, true)
        test("normalCat") - check(name => BoxWith(NormalCat(name)), normalCat, true)
        test("bigCat") - check(name => BoxWith(BigCat(name)), bigCat, true)
        test("veryBigCat") - check(name => BoxWith(VeryBigCat(name)), veryBigCat, true)
      }
    }
  }
}
