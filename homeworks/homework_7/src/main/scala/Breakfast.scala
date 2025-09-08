  def makeBreakfast(eggsFiringTime: Duration,
                    waterBoilingTime: Duration,
                    saladInfoTime: SaladInfoTime,
                    teaBrewingTime: Duration): ZIO[Any, Throwable, Map[String, LocalDateTime]] = ???
                    teaBrewingTime: Duration): ZIO[Any, Throwable, Map[String, LocalDateTime]] =  {

    for {
      water <- ZIO.sleep(waterBoilingTime).as(LocalDateTime.now()).fork
      eggs <- ZIO.sleep(eggsFiringTime).as(LocalDateTime.now()).fork
      salad <- ZIO.sleep(saladInfoTime.tomatoTime.plus(saladInfoTime.cucumberTime)).as(LocalDateTime.now()).fork

      waterTime <- water.join

      tea <- ZIO.sleep(teaBrewingTime).as(LocalDateTime.now()).fork

      eggsTime <- eggs.join
      saladTime <- salad.join
      teaTime <- tea.join

    } yield Map(
      "eggs" -> eggsTime,
      "water" -> waterTime,
      "saladWithSourCream" -> saladTime,
      "tea" -> teaTime
    )
  }
