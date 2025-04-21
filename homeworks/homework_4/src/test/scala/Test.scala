import utest._

import scala.util.Random

object Test extends TestSuite {
    lazy val randomLength = Random
    def generateRandomList(maxListSize: Int) = {
        val listLength = Random.nextInt(maxListSize)
        List.fill(listLength)(Random.nextInt)
    }
    override def tests: Tests = Tests {
        'firstTask - (1 to 5).foreach { _ =>
            val testList = generateRandomList(50)
            val sumValue = testList(Random.nextInt(testList.size)) + testList(Random.nextInt(testList.size))
            assert(Exercises.findSumImperative(testList, sumValue) == Exercises.findSumFunctional(testList, sumValue))
        }

        'recursionTask - {
            'itWorks - {
                (1 to 5).foreach { _ =>
                    val testList = generateRandomList(50)
                    assert(Exercises.simpleRecursion(testList) == Exercises.tailRecRecursion(testList))
                }
            }
            'longList - {
                val testList = List.fill(10000)(Random.nextInt)
                Exercises.tailRecRecursion(testList)
            }
        }

        'binarySearch - {
            'simpleSearch - (1 to 5).foreach { _ =>
                val testList = generateRandomList(50).distinct.sorted
                val result = Random.nextInt(testList.size)
                assert(Exercises.functionalBinarySearch(testList, testList(result)).contains(result))
            }

            'empty - {
                assert(Exercises.functionalBinarySearch(Nil, 24).isEmpty)
            }

            'noElement - {
                val testList = List.fill(50)(Random.nextInt(50))
                assert(Exercises.functionalBinarySearch(testList, -1).isEmpty)
            }
        }

        'namesList - {
            'nonEmpty - (1 to 5).foreach { _ =>
                val namesCount = Random.nextInt(50)
                val names = Exercises.generateNames(namesCount)
                assert(
                    names.forall(_.matches("([A-Z]|[А-Я])([a-z]|[а-я])*")) &&
                        names.size == namesCount &&
                        names.distinct.size == namesCount
                )
            }

            'empty - {
                assert(Exercises.generateNames(0).isEmpty)
            }
        }
    }
}
