package ru.dru

import zio.{Clock, Duration, Exit, Fiber, Scope, ZIO, ZIOApp, ZIOAppArgs, ZIOAppDefault, durationInt}
import zio.Clock.currentDateTime

import java.time.LocalDateTime
import scala.concurrent.TimeoutException

case class SaladInfoTime(tomatoTime: Duration, cucumberTime: Duration)

object Breakfast extends ZIOAppDefault {

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