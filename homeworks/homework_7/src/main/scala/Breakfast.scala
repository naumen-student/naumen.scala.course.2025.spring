package ru.dru

import zio.CanFail.canFailAmbiguous1
import zio.{Duration, Exit, Fiber, Scope, ZIO, ZIOApp, ZIOAppArgs, ZIOAppDefault, durationInt}

import java.time.LocalDateTime
import scala.concurrent.TimeoutException

import zio._
import java.time.LocalDateTime


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
      start <- ZIO.succeed(LocalDateTime.now())

      // параллельно: жарим яйца и кипятим воду
      eggsFiber  <- (ZIO.sleep(eggsFiringTime) *> ZIO.succeed("eggs")).fork
      waterFiber <- (ZIO.sleep(waterBoilingTime) *> ZIO.succeed("water")).fork

      // параллельно: готовим салат
      saladFiber <- (
        for {
          _ <- ZIO.sleep(saladInfoTime.cucumberTime)
          _ <- ZIO.sleep(saladInfoTime.tomatoTime)
          _ <- ZIO.unit // добавляем сметану (мгновенно)
          _ <- ZIO.succeed(())
        } yield "saladWithSourCream"
        ).fork

      // дожидаемся кипячения воды и завариваем чай
      water <- waterFiber.join
      _ <- ZIO.sleep(teaBrewingTime)
      tea <- ZIO.succeed("tea")

      // дожидаемся остальных
      eggs <- eggsFiber.join
      salad <- saladFiber.join

      now <- ZIO.succeed(LocalDateTime.now())

      result <- ZIO.succeed(Map(
        eggs -> start.plus(eggsFiringTime),
        water -> start.plus(waterBoilingTime),
        salad -> start.plus(saladInfoTime.cucumberTime + saladInfoTime.tomatoTime),
        tea -> start.plus(waterBoilingTime + teaBrewingTime)
      ))

    } yield result
  }


  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed(println("Done"))

}