package ru.dru

import zio._
import zio.Clock

import java.time.{LocalDateTime, OffsetDateTime}

case class SaladInfoTime(tomatoTime: Duration, cucumberTime: Duration)


object Breakfast extends ZIOAppDefault {

  def makeBreakfast(
                     eggsFiringTime: Duration,
                     waterBoilingTime: Duration,
                     saladInfoTime: SaladInfoTime,
                     teaBrewingTime: Duration
                   ): ZIO[Any, Throwable, Map[String, LocalDateTime]] = {

    // Преобразование OffsetDateTime в LocalDateTime
    def currentLocalDateTime: ZIO[Any, Nothing, LocalDateTime] =
      Clock.currentDateTime.map(_.toLocalDateTime)

    val waterAndTea = for {
      _ <- ZIO.sleep(waterBoilingTime)
      waterTime <- currentLocalDateTime
      _ <- ZIO.sleep(teaBrewingTime)
      teaTime <- currentLocalDateTime
    } yield Map("water" -> waterTime, "tea" -> teaTime)

    val eggsEffect = for {
      _ <- ZIO.sleep(eggsFiringTime)
      eggsTime <- currentLocalDateTime
    } yield Map("eggs" -> eggsTime)

    val saladEffect = for {
      _ <- ZIO.sleep(saladInfoTime.cucumberTime)
      _ <- ZIO.sleep(saladInfoTime.tomatoTime)
      saladTime <- currentLocalDateTime
    } yield Map("saladWithSourCream" -> saladTime)

    // Параллельное комбинирование и объединение результатов
    waterAndTea.zipWithPar(eggsEffect)(_ ++ _).zipWithPar(saladEffect)(_ ++ _)
  }


  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed(println("Done"))

}
