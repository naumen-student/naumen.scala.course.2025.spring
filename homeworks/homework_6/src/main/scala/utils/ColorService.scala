package utils

import utils.Utils.GenerationError
import zio.{IO, Random, Task, ULayer, ZIO, ZLayer}

import java.awt.Color
import scala.util.Try

object ColorService {

    type ColorService = Service
    trait Service {
        def generateRandomColor(): IO[GenerationError, Color]
        def getColor(r: Int, g: Int, b: Int): Task[Color]
    }

    class ServiceImpl extends Service {
        override def generateRandomColor(): IO[GenerationError, Color] =
            (Random.nextIntBetween(0, 255) <*> Random.nextIntBetween(0, 255) <*> Random.nextIntBetween(0, 255))
                .flatMap { case (r, g, b) => getColor(r, g, b) }
                .mapError(e => new GenerationError(e.getMessage))

        override def getColor(r: Int, g: Int, b: Int): Task[Color] =
            ZIO.fromTry(Try(new Color(r, g, b)))
    }

    val live: ULayer[ColorService] = ZLayer.succeed(new ServiceImpl)
}
