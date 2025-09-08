import utils.ColorService.ColorService
import utils.{ColorService, PictureGenerationService}
import utils.PictureGenerationService.PictureGenerationService
import utils.Utils._
import zio.{IO, Random, URIO, ZIO}
object Exercises {

    /**
     * В задании необходимо модифицировать ZIO объект, чтобы в случае ошибки в методе getColor
     * вернулся None, а в случае упеха Some
     * вернулся None, а в случае успеха Some
     */
    def task1(r: Int, g: Int, b: Int): URIO[ColorService, Option[Color]] =
        ZIO.serviceWithZIO[ColorService](_.getColor(r, g, b))
        ZIO.serviceWithZIO[ColorService](_.getColor(r, g, b).option)


    /**
     * Неободимо модифицировать ZIO объект так, чтобы он возвращал текстовую матрицу цветов вида
     * Необходимо модифицировать ZIO объект так, чтобы он возвращал текстовую матрицу цветов вида
     * 1 23 -4
     * 25 -1 2
     * где элементы - числовые значения объекта Color (можно получить через getRGB)
     */
    def task2(size: (Int, Int)): ZIO[PictureGenerationService, GenerationError, String] =
        ZIO.serviceWithZIO[PictureGenerationService](_.generatePicture(size))
        ZIO.serviceWithZIO[PictureGenerationService](_.generatePicture(size).map(p => {
            p.lines.map(l => l.map(el => -el.getRGB).mkString(" ")).mkString("\n")
        }))


object Exercises {
        for {
            colorServ <- ZIO.service[ColorService]
            pictureServ <- ZIO.service[PictureGenerationService]
            color <- colorServ.generateRandomColor()
            picture <- pictureServ.generatePicture(size)
            filledPicture <- pictureServ.fillPicture(picture, color)
            color <- colorServ.generateRandomColor().mapError(_ => new GenerationError("Не удалось создать цвет"))
            picture <- pictureServ.generatePicture(size).mapError(_ => new GenerationError("Ошибка генерации изображения"))
            filledPicture <- pictureServ.fillPicture(picture, color).mapError(_ => new GenerationError("Возникли проблемы при заливке изображения"))
        } yield filledPicture

    /**
     * Необходимо предоставить объекту ZIO все необходимые зависимости
     */
    def task4(size: (Int, Int)): IO[GenerationError, Picture] =
        task3(size)

          .provideSomeLayer(PictureGenerationService.live)
          .provideLayer(ColorService.live)
}
