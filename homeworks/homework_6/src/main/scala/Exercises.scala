import zio._
import zio.mock.Expectation
import utils.Utils.{GenerationError, Picture}
import utils.{ColorService, PictureGenerationService}
import java.awt.Color

object Exercises {

    def task1(r: Int, g: Int, b: Int): URIO[ColorService.ColorService, Option[Color]] =
        ZIO.serviceWithZIO[ColorService.ColorService](_.getColor(r, g, b)).option


    def task2(size: (Int, Int)): ZIO[PictureGenerationService.PictureGenerationService, GenerationError, String] =
        ZIO.serviceWithZIO[PictureGenerationService.PictureGenerationService](_.generatePicture(size)).map { picture =>
            picture.lines.map(row =>
                row.map(color => math.abs(color.getRGB).toString).mkString(" ")
            ).mkString("\n")
        }


    def task3(size: (Int, Int)): ZIO[PictureGenerationService.PictureGenerationService with ColorService.ColorService, GenerationError, Picture] =
        for {
            colorServ <- ZIO.service[ColorService.ColorService]
            pictureServ <- ZIO.service[PictureGenerationService.PictureGenerationService]
            color <- colorServ.generateRandomColor().mapError(_ => new GenerationError("Не удалось создать цвет"))
            picture <- pictureServ.generatePicture(size).mapError(_ => new GenerationError("Ошибка генерации изображения"))
            filledPicture <- pictureServ.fillPicture(picture, color).mapError(_ => new GenerationError("Возникли проблемы при заливке изображения"))
        } yield filledPicture


        task3(size).provide(ColorService.live, PictureGenerationService.live)
}

