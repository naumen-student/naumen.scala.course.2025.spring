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
  def makeBreakfast(eggsFiringTime: Duration,
                    waterBoilingTime: Duration,
                    saladInfoTime: SaladInfoTime,
                    teaBrewingTime: Duration): ZIO[Any, Throwable, Map[String, LocalDateTime]] = {
    
    val eggsProcess = for {
      _ <- ZIO.sleep(eggsFiringTime)
      eggsFinishTime <- ZIO.succeed(LocalDateTime.now())
    } yield ("eggs", eggsFinishTime)
    
    val waterAndTeaProcess = for {
      _ <- ZIO.sleep(waterBoilingTime)
      waterFinishTime <- ZIO.succeed(LocalDateTime.now())
      _ <- ZIO.sleep(teaBrewingTime)
      teaFinishTime <- ZIO.succeed(LocalDateTime.now())
    } yield List(("water", waterFinishTime), ("tea", teaFinishTime))
    
    val saladProcess = for {
      _ <- ZIO.sleep(saladInfoTime.cucumberTime)
      _ <- ZIO.sleep(saladInfoTime.tomatoTime)
      saladFinishTime <- ZIO.succeed(LocalDateTime.now())
    } yield ("saladWithSourCream", saladFinishTime)
    
    for {
      eggs <- eggsProcess.fork
      waterAndTea <- waterAndTeaProcess.fork
      salad <- saladProcess.fork
      
      eggsResult <- eggs.join
      waterAndTeaResult <- waterAndTea.join
      saladResult <- salad.join
      
    } yield Map(
      eggsResult,
      saladResult
    ) ++ waterAndTeaResult.toMap
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed(println("Done"))

}
