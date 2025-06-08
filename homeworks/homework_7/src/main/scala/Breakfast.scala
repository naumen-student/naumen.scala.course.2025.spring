package ru.dru

import zio.CanFail.canFailAmbiguous1
import zio.{Duration, Exit, Fiber, Scope, ZIO, ZIOApp, ZIOAppArgs, ZIOAppDefault, durationInt}

import java.time.LocalDateTime
import scala.concurrent.TimeoutException

case class SaladInfoTime(tomatoTime: Duration, cucumberTime: Duration)



object Breakfast extends ZIOAppDefault {

  /**
@@ -32,10 +29,33 @@ object Breakfast extends ZIOAppDefault {
  def makeBreakfast(eggsFiringTime: Duration,
                    waterBoilingTime: Duration,
                    saladInfoTime: SaladInfoTime,
                    teaBrewingTime: Duration): ZIO[Any, Throwable, Map[String, LocalDateTime]] = ???



  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed(println("Done"))

}
                    teaBrewingTime: Duration): ZIO[Any, Throwable, Map[String, LocalDateTime]] = {
    for {
      start <- ZIO.succeed(LocalDateTime.now())
      water <- ZIO.sleep(waterBoilingTime).fork
      eggs <- ZIO.sleep(eggsFiringTime).fork
      salad <- (ZIO.sleep(saladInfoTime.cucumberTime) *> ZIO.sleep(saladInfoTime.tomatoTime)).fork
      tea <- ZIO.sleep(waterBoilingTime.plus(teaBrewingTime))
      end <- ZIO.succeed(LocalDateTime.now())
    } yield Map (
      "eggs" -> start.plusSeconds(eggsFiringTime.toSeconds),
      "water" -> start.plusSeconds(waterBoilingTime.toSeconds),
      "saladWithSourCream" -> start.plusSeconds(saladInfoTime.cucumberTime.toSeconds + saladInfoTime.tomatoTime.toSeconds),
      "tea" -> start.plusSeconds(waterBoilingTime.toSeconds + teaBrewingTime.toSeconds)
    )
  }


  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val eggsFiringTime = 5.seconds
    val waterBoilingTime = 3.seconds
    val saladInfoTime = SaladInfoTime(1.seconds, 3.seconds)
    val teaBrewingTime = 3.seconds

    makeBreakfast(eggsFiringTime, waterBoilingTime, saladInfoTime, teaBrewingTime).fold(
      error => println(s"Error occurred: $error"),
      result => println(s"Breakfast is ready! $result")
    )
  }

}
