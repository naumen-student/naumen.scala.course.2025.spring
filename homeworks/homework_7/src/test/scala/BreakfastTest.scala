package ru.dru

import zio.{Clock, Scope, ZIO, durationInt}
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertCompletesZIO, assertTrue}

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import zio.internal.stacktracer.Tracer._
import zio.test.Assertion._

object BreakfastTest extends ZIOSpecDefault {

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Breakfast") (
    test("test1") {
      for {
        now <- ZIO.succeed(LocalDateTime.now())


        breakfast <- Breakfast.makeBreakfast(
          10.seconds,
          5.seconds,
          SaladInfoTime(
            7.seconds,
            7.seconds
          ),
          1.seconds).map(_.transform((_, time) => time.truncatedTo(ChronoUnit.SECONDS))).withClock(Clock.ClockLive)

        correct <- ZIO.succeed(Map(
          "eggs" -> now.plusSeconds(10),
          "water" -> now.plusSeconds(5),
          "saladWithSourCream" -> now.plusSeconds(14),
          "tea" -> now.plusSeconds(6),
        ).transform((_, time) => time.truncatedTo(ChronoUnit.SECONDS)))

      } yield assertTrue {
        breakfast == correct
      }
    },
    test("test2") {
      for {
        now <- ZIO.succeed(LocalDateTime.now())


        breakfast <- Breakfast.makeBreakfast(
          5.seconds,
          5.seconds,
          SaladInfoTime(
            5.seconds,
            5.seconds
          ),
          5.seconds).map(_.transform((_, time) => time.truncatedTo(ChronoUnit.SECONDS))).withClock(Clock.ClockLive)

        correct <- ZIO.succeed(Map(
          "eggs" -> now.plusSeconds(5),
          "water" -> now.plusSeconds(5),
          "saladWithSourCream" -> now.plusSeconds(10),
          "tea" -> now.plusSeconds(10),
        ).transform((_, time) => time.truncatedTo(ChronoUnit.SECONDS)))

      } yield assertTrue {
        breakfast == correct
      }
    }
  )
}
