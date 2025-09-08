package ru.dru

import zio.CanFail.canFailAmbiguous1
import zio.{Duration, Exit, Fiber, Scope, ZIO, ZIOApp, ZIOAppArgs, ZIOAppDefault, durationInt}

import java.time.LocalDateTime
import scala.concurrent.TimeoutException

case class SaladInfoTime(tomatoTime: Duration, cucumberTime: Duration)

object Breakfast extends ZIOAppDefault {

  /**
   * Функция должна эмулировать приготовление завтрака. Продолжительные операции необходимо эмулировать через ZIO.sleep.
   * Правила приготовления следующие:
   *  1. Нобходимо вскипятить воду (время кипячения waterBoilingTime)
   *  2. Параллельно с этим нужно жарить яичницу eggsFiringTime
   *  3. Параллельно с этим готовим салат:
   *    * сначала режим  огурцы
   *    * после этого режим помидоры
   *    * после этого добавляем в салат сметану
   *  4. После того, как закипит вода необходимо заварить чай, время заваривания чая teaBrewingTime
   *  5. После того, как всё готово, можно завтракать
   *
   * @param eggsFiringTime время жарки яичницы
   * @param waterBoilingTime время кипячения воды
   * @param saladInfoTime информация о времени для приготовления салата
   * @param teaBrewingTime время заваривания чая
   * @return Мапу с информацией о том, когда завершился очередной этап (eggs, water, saladWithSourCream, tea)
   */
  def makeBreakfast(
                     eggsFiringTime: Duration,
                     waterBoilingTime: Duration,
                     saladInfoTime: SaladInfoTime,
                     teaBrewingTime: Duration
                   ): ZIO[Any, Throwable, Map[String, LocalDateTime]] = {

    def logCompletion(stage: String): ZIO[Any, Nothing, LocalDateTime] =
      ZIO.succeed(LocalDateTime.now())

    val fryEggs = for {
      _       <- ZIO.sleep(eggsFiringTime)
      doneAt  <- logCompletion("eggs")
    } yield "eggs" -> doneAt

    val boilWaterAndBrewTea = for {
      _         <- ZIO.sleep(waterBoilingTime)
      waterDone <- logCompletion("water")
      _         <- ZIO.sleep(teaBrewingTime)
      teaDone   <- logCompletion("tea")
    } yield List("water" -> waterDone, "tea" -> teaDone)

    val makeSalad = for {
      _       <- ZIO.sleep(saladInfoTime.cucumberTime)
      _       <- ZIO.sleep(saladInfoTime.tomatoTime)
      doneAt  <- logCompletion("saladWithSourCream")
    } yield "saladWithSourCream" -> doneAt

    for {
      fiberEggs    <- fryEggs.fork
      fiberWaterTea <- boilWaterAndBrewTea.fork
      fiberSalad   <- makeSalad.fork

      eggsResult   <- fiberEggs.join
      waterTeaResult <- fiberWaterTea.join
      saladResult  <- fiberSalad.join
    } yield Map(eggsResult, saladResult) ++ waterTeaResult.toMap
  }

  override def run: ZIO[Any, Throwable, Int] = {
    val eggsTime   = 3.seconds
    val waterTime  = 5.seconds
    val teaTime    = 4.seconds
    val saladTimes = SaladInfoTime(cucumberTime = 2.seconds, tomatoTime = 2.seconds)

    makeBreakfast(eggsTime, waterTime, saladTimes, teaTime).flatMap { result =>
      ZIO.foreach(result.toList) { case (stage, time) =>
        ZIO.succeed(println(s"$stage completed at: $time"))
      }.as(0)
    }
  }
}
