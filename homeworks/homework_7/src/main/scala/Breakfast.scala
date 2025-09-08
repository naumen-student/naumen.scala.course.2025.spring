package ru.dru

import zio.{Clock, Duration, Scope, ZIO, ZIOAppArgs, ZIOAppDefault, durationInt}
import java.time.LocalDateTime

case class SaladInfoTime(tomatoTime: Duration, cucumberTime: Duration)

object Breakfast extends ZIOAppDefault {

  def makeBreakfast(
                     eggsFiringTime: Duration,
                     waterBoilingTime: Duration,
                     saladInfoTime: SaladInfoTime,
                     teaBrewingTime: Duration
                   ): ZIO[Any, Throwable, Map[String, LocalDateTime]] = {

    val eggsTask =
      ZIO.sleep(eggsFiringTime) *> ZIO.succeed(LocalDateTime.now())

    val waterTask =
      ZIO.sleep(waterBoilingTime) *> ZIO.succeed(LocalDateTime.now())

    val saladTask =
      ZIO.sleep(saladInfoTime.cucumberTime) *>
        ZIO.sleep(saladInfoTime.tomatoTime) *>
        ZIO.succeed(LocalDateTime.now())

    for {
      eggsFiber  <- eggsTask.fork
      waterFiber <- waterTask.fork
      saladFiber <- saladTask.fork
      waterTime  <- waterFiber.join
      teaFiber   <- (ZIO.sleep(teaBrewingTime) *> ZIO.succeed(LocalDateTime.now())).fork
      eggsTime   <- eggsFiber.join
      saladTime  <- saladFiber.join
      teaTime    <- teaFiber.join
    } yield Map(
      "eggs"                -> eggsTime,
      "water"               -> waterTime,
      "saladWithSourCream"  -> saladTime,
      "tea"                 -> teaTime
    )
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    ZIO.succeed(println("Done"))
}