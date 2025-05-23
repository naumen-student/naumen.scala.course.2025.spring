package ru.dru

import zio.CanFail.canFailAmbiguous1
import zio.{Clock, Duration, Exit, Fiber, Scope, ZIO, ZIOApp, ZIOAppArgs, ZIOAppDefault, durationInt}

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
      startTime <- Clock.currentDateTime
      
      // Запускаем параллельно: яичницу, воду и салат
      eggsFiber <- (ZIO.sleep(eggsFiringTime) *> Clock.currentDateTime.map(_.toLocalDateTime)).fork
      waterFiber <- (ZIO.sleep(waterBoilingTime) *> Clock.currentDateTime.map(_.toLocalDateTime)).fork
      
      // Салат готовится последовательно: огурцы -> помидоры -> сметана
      saladFiber <- (for {
        _ <- ZIO.sleep(saladInfoTime.cucumberTime) // режем огурцы
        _ <- ZIO.sleep(saladInfoTime.tomatoTime)   // режем помидоры
        // добавляем сметану (мгновенно)
        finishTime <- Clock.currentDateTime.map(_.toLocalDateTime)
      } yield finishTime).fork
      
      // Ждем когда закипит вода
      waterTime <- waterFiber.join
      
      // После кипячения воды завариваем чай
      teaTime <- ZIO.sleep(teaBrewingTime) *> Clock.currentDateTime.map(_.toLocalDateTime)
      
      // Ждем готовности яичницы и салата
      eggsTime <- eggsFiber.join
      saladTime <- saladFiber.join
      
    } yield Map(
      "eggs" -> eggsTime,
      "water" -> waterTime,
      "saladWithSourCream" -> saladTime,
      "tea" -> teaTime
    )
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed(println("Done"))

}
