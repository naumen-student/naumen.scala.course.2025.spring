package ru.dru

import zio.{Clock, Duration, Exit, Fiber, Scope, ZIO, ZIOApp, ZIOAppArgs, ZIOAppDefault, durationInt}
import zio.Clock.currentDateTime

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
    for {
      now <- ZIO.succeed(LocalDateTime.now())
      eggsFiber <- (ZIO.sleep(eggsFiringTime) *> ZIO.succeed(LocalDateTime.now())).fork
      waterFiber <- (ZIO.sleep(waterBoilingTime) *> ZIO.succeed(LocalDateTime.now())).fork
      saladFiber <- (for {
        _ <- ZIO.sleep(saladInfoTime.cucumberTime)
        _ <- ZIO.sleep(saladInfoTime.tomatoTime)
        saladTime <- ZIO.succeed(LocalDateTime.now())
      } yield saladTime).fork

      waterTime <- waterFiber.join
      teaFiber <- (ZIO.sleep(teaBrewingTime) *> ZIO.succeed(LocalDateTime.now())).fork

      eggsTime <- eggsFiber.join
      saladTime <- saladFiber.join
      teaTime <- teaFiber.join
      result = Map(
        "eggs" -> eggsTime,
        "water" -> waterTime,
        "saladWithSourCream" -> saladTime,
        "tea" -> teaTime
      )
    } yield result
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed(println("Done"))
}