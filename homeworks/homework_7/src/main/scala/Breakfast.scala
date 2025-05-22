package ru.dru

import zio.{Duration, Fiber, Scope, UIO, ZIO, ZIOAppArgs, ZIOAppDefault, durationInt}
import java.time.LocalDateTime

case class SaladInfoTime(cucumberTime: Duration, tomatoTime: Duration)

object Breakfast extends ZIOAppDefault {

  private def currentTime: UIO[LocalDateTime] = ZIO.succeed(LocalDateTime.now())

  private def boilWater(waterBoilingTime: Duration): UIO[LocalDateTime] =
    ZIO.sleep(waterBoilingTime) *> currentTime

  private def fryEggs(eggsFiringTime: Duration): UIO[LocalDateTime] =
    ZIO.sleep(eggsFiringTime) *> currentTime

  private def prepareSalad(saladInfoTime: SaladInfoTime): UIO[LocalDateTime] =
    ZIO.sleep(saladInfoTime.cucumberTime) *>
      ZIO.sleep(saladInfoTime.tomatoTime) *>
      currentTime

  private def brewTea(teaBrewingTime: Duration): UIO[LocalDateTime] =
    ZIO.sleep(teaBrewingTime) *> currentTime

  def makeBreakfast(
                     eggsFiringTime: Duration,
                     waterBoilingTime: Duration,
                     saladInfoTime: SaladInfoTime,
                     teaBrewingTime: Duration
                   ): ZIO[Any, Nothing, Map[String, LocalDateTime]] = {
    for {
      waterFiber  <- boilWater(waterBoilingTime).fork
      eggsFiber   <- fryEggs(eggsFiringTime).fork
      saladFiber  <- prepareSalad(saladInfoTime).fork

      waterTime   <- waterFiber.join
      teaFiber    <- brewTea(teaBrewingTime).fork

      eggsTime    <- eggsFiber.join
      saladTime   <- saladFiber.join
      teaTime     <- teaFiber.join
    } yield Map(
      "eggs" -> eggsTime,
      "water" -> waterTime,
      "saladWithSourCream" -> saladTime,
      "tea" -> teaTime
    )
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val eggsTime = 5.seconds
    val waterTime = 10.seconds
    val saladTime = SaladInfoTime(3.seconds, 4.seconds)
    val teaTime = 2.seconds

    makeBreakfast(eggsTime, waterTime, saladTime, teaTime)
      .flatMap(result => ZIO.succeed(println(result)))
  }
}