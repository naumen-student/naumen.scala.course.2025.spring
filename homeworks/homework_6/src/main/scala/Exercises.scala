import utils.ColorService.ColorService
import utils.PictureGenerationService.PictureGenerationService
import utils.Utils._
import zio.{IO, Random, URIO, ZIO}

import java.awt.Color

object Exercises6 {

    /**
     * В задании необходимо модифицировать ZIO объект, чтобы в случае ошибки в методе getColor
     * вернулся None, а в случае упеха Some
     */
    def task1(r: Int, g: Int, b: Int): URIO[ColorService, Option[Color]] =
        ZIO.serviceWithZIO[ColorService](_.getColor(r, g, b)).option


    /**
     * Неободимо модифицировать ZIO объект так, чтобы он возвращал текстовую матрицу цветов вида
     * 1 23 -4
     * 25 -1 2
     * где элементы - числовые значения объекта Color (можно получить через getRGB)
     */
    def task2(size: (Int, Int)): ZIO[PictureGenerationService, GenerationError, String] =
        ZIO.serviceWithZIO[PictureGenerationService](_.generatePicture(size)).map { picture =>
            val rows = picture.lines.map(line => 
                line.map(color => Math.abs(color.getRGB())).mkString(" ")
            )
            rows.mkString("\n")
        }


    /**
     * В задаче необходимо поработать с ошибками
     * 1. Необходимо, чтобы тип ошибки был единым для всего объекта ZIO, иначе не соберется
     * 2. Необходимо добавить в случае каждой из ошибок возвращать ее с определенным текстом
     *  - при ошибке генерации случайного цвета -> Не удалось создать цвет
     *  - при генерации картинки -> Ошибка генерации изображения
     *  - при заполнении картинки -> Возникли проблемы при заливке изображения
     */
    def task3(size: (Int, Int)): ZIO[PictureGenerationService with ColorService, GenerationError, Picture] = {
        val colorService = ZIO.service[ColorService]
        val pictureService = ZIO.service[PictureGenerationService]
        
        val color = colorService.flatMap(_.generateRandomColor())
            .mapError(_ => new GenerationError("Не удалось создать цвет"))
            
        val picture = pictureService.flatMap(_.generatePicture(size))
            .mapError(_ => new GenerationError("Ошибка генерации изображения"))
            
        val filledPicture = for {
            c <- color
            p <- picture
            serv <- pictureService
            result <- serv.fillPicture(p, c)
                .mapError(_ => new GenerationError("Возникли проблемы при заливке изображения"))
        } yield result

        filledPicture
    }

    /**
     * Необходимо предоставить объекту ZIO все необходимые зависимости
     */
    def task4(size: (Int, Int)): IO[GenerationError, Picture] =
        task3(size).provideLayer(ColorService.live >+> PictureGenerationService.live)

}
