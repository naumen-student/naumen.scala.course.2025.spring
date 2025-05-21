import utils.ColorService.ColorService
import utils.{ColorService, PictureGenerationService}
import utils.PictureGenerationService.PictureGenerationService
import utils.Utils._
import zio.{IO, Random, URIO, ZIO}

import java.awt.Color

object Exercises {

    /**
     * В задании необходимо модифицировать ZIO объект, чтобы в случае ошибки в методе getColor
     * вернулся None, а в случае упеха Some
     */
    def task1(r: Int, g: Int, b: Int): URIO[ColorService, Option[Color]] =
        ZIO.serviceWithZIO[ColorService](_.getColor(r, g, b).option)


    /**
     * Неободимо модифицировать ZIO объект так, чтобы он возвращал текстовую матрицу цветов вида
     * 1 23 -4
     * 25 -1 2
     * где элементы - числовые значения объекта Color (можно получить через getRGB)
     */
    def task2(size: (Int, Int)): ZIO[PictureGenerationService, GenerationError, String] = {
        ZIO.serviceWithZIO[PictureGenerationService](_.generatePicture(size))
          .map(q => q.lines
            .map(
                _.map(w => Integer.toUnsignedString(w.getRGB))
                  .mkString(" "))
            .mkString("\n"))
    }


    /**
     * В задаче необходимо поработать с ошибками
     * 1. Необходимо, чтобы тип ошибки был единым для всего объекта ZIO, иначе не соберется
     * 2. Необходимо добавить в случае каждой из ошибок возвращать ее с определенным текстом
     *  - при ошибке генерации случайного цвета -> Не удалось создать цвет
     *  - при генерации картинки -> Ошибка генерации изображения
     *  - при заполнении картинки -> Возникли проблемы при заливке изображения
     */
    def task3(size: (Int, Int)): ZIO[PictureGenerationService with ColorService, GenerationError, Picture] =
        for {
            colorServ <- ZIO.service[ColorService]
            pictureServ <- ZIO.service[PictureGenerationService]
            color <- colorServ.generateRandomColor()
              .mapError(_ => new GenerationError("Не удалось создать цвет"))
            picture <- pictureServ.generatePicture(size)
              .mapError(_ => new GenerationError("Ошибка генерации изображения"))
            filledPicture <- pictureServ.fillPicture(picture, color)
              .mapError(_ => new GenerationError("Возникли проблемы при заливке изображения"))
        } yield filledPicture

    /**
     * Необходимо предоставить объекту ZIO все необходимые зависимости
     */
    def task4(size: (Int, Int)): IO[GenerationError, Picture] =
        task3(size).provide(PictureGenerationService.live, ColorService.live)

}
