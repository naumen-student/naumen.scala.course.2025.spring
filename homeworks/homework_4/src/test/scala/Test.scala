import utest._

import scala.util.Random

object Test extends TestSuite {
    lazy val randomLength: Random.type = Random
    def generateRandomList(maxListSize: Int): List[Int] = {
        val listLength = Random.nextInt(maxListSize)
        List.fill(listLength)(Random.nextInt)
    }
    override def tests: Tests = Tests {

        test("firstTask") - (1 to 5).foreach { _ =>
            val testList = generateRandomList(50)
            val sumValue = testList(Random.nextInt(testList.size)) + testList(Random.nextInt(testList.size))
            assert(Exercises.findSumImperative(testList, sumValue) == Exercises.findSumFunctional(testList, sumValue))
        }

        test("recursionTask") {

            test("itWorks") {
                (1 to 5).foreach { _ =>
                    val testList = generateRandomList(50)
                    val simpleRec = Exercises.simpleRecursion(testList)
                    val tailRec = Exercises.tailRecRecursion(testList)
                    assert(simpleRec == tailRec)
                    //assert(Exercises.simpleRecursion(testList) == Exercises.tailRecRecursion(testList))
                }
            }

            test("longList") {
                val testList = List.fill(10000)(Random.nextInt)
                Exercises.tailRecRecursion(testList)
            }
        }

        test("binarySearch") {

            test("simpleSearch") - (1 to 5).foreach { _ =>
                val testList = generateRandomList(50).distinct.sorted
                val result = Random.nextInt(testList.size)
                val BinarySearch = Exercises.functionalBinarySearch(testList, testList(result))
                assert(BinarySearch.contains(result))
                //assert(Exercises.functionalBinarySearch(testList, testList(result)).contains(result))
            }

            test("empty") - {
                assert(Exercises.functionalBinarySearch(Nil, 24).isEmpty)
            }

            test("noElement") {
                val testList = List.fill(50)(Random.nextInt(50))
                assert(Exercises.functionalBinarySearch(testList, -1).isEmpty)
            }
        }

        test("namesList")  {

            test("nonEmpty") - (1 to 5).foreach { _ =>
                val namesCount = Random.nextInt(50)
                val names = Exercises.generateNames(namesCount)
                assert(
                    names.forall(_.matches("([A-Z]|[А-Я])([a-z]|[а-я])*")) &&
                        names.size == namesCount &&
                        names.distinct.size == namesCount
                )
            }

            test("empty") {
                assert(Exercises.generateNames(0).isEmpty)
            }
        }
    }
}
