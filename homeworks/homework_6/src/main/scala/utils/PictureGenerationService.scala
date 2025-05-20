package utils

import utils.Utils.{GenerationError, Picture}
import zio.{IO, Random, Task, ZIO, ZLayer}

import java.awt.Color

object PictureGenerationService {
    type PictureGenerationService = Service

    trait Service {
        def generatePicture(size: (Int, Int)): IO[GenerationError, Picture]

        def fillPicture(picture: Picture, color: Color): Task[Picture]

    }

    class ServiceImpl(colorService: ColorService.Service) extends Service {
        override def generatePicture(size: (Int, Int)): IO[GenerationError, Picture] = {
            ZIO.foreach((1 to size._1).toList) { _ =>
                ZIO.foreach((1 to size._2).toList) { _ =>
                    colorService.generateRandomColor()
                }
            }.map(list => Picture(list))
        }

        override def fillPicture(picture: Picture, color: Color): Task[Picture] = {
            ZIO.succeed(picture.copy(
                lines = picture.lines.map(_.map(_ => color))
            ))
        }
    }

    val live: ZLayer[ColorService.Service, Nothing, PictureGenerationService] = ZLayer.fromFunction(new ServiceImpl(_))
}
