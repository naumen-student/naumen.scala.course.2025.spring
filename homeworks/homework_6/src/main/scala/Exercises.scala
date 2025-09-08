import utils.ColorService.ColorService
import utils.PictureGenerationService.PictureGenerationService
import utils.Utils._
import zio.{IO, URIO, ZIO, ZLayer}

import java.awt.Color

object Exercises {

    def task1(r: Int, g: Int, b: Int): URIO[ColorService, Option[Color]] =
        ZIO.serviceWithZIO[ColorService](_.getColor(r, g, b).option)

    def task2(size: (Int, Int)): ZIO[PictureGenerationService, GenerationError, String] =
        ZIO.serviceWithZIO[PictureGenerationService](_.generatePicture(size))
          .map(_.lines.map(_.map(colour => Integer.toUnsignedString(colour.getRGB)).mkString(" ")).mkString("\n"))

    def task3(size: (Int, Int)): ZIO[PictureGenerationService with ColorService, GenerationError, Picture] =
        for {
            color <- ZIO.serviceWithZIO[ColorService](
                _.generateRandomColor().mapError(_ => new GenerationError("Не удалось создать цвет"))
            )
            picture <- ZIO.serviceWithZIO[PictureGenerationService](
                _.generatePicture(size).mapError(_ => new GenerationError("Ошибка генерации изображения"))
            )
            filled <- ZIO.serviceWithZIO[PictureGenerationService](
                _.fillPicture(picture, color).mapError(_ => new GenerationError("Возникли проблемы при заливке изображения"))
            )
        } yield filled

    def task4(size: (Int, Int)): IO[GenerationError, Picture] =
        task3(size).provide(
            utils.ColorService.live,
            utils.PictureGenerationService.live
        )
}