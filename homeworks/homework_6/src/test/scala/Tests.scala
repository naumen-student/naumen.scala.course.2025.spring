import Mocks.{ColorServiceMock, GeneratePictureServiceMock}
import utils.Utils.{GenerationError, Picture}
import utils.{ColorService, PictureGenerationService}
import zio.mock.Expectation
import zio.{Scope, ZLayer}
import zio.test._

import java.awt.Color

object Tests extends ZIOSpecDefault {

    val fullLayer = ColorService.live >+> PictureGenerationService.live
    val layerWithFailedRandomColor =
        ColorServiceMock.GenerateRandomColor(returns = Expectation.failure(new GenerationError(""))).toLayer >+>
            PictureGenerationService.live

    val layerWithFailedGeneratePicture = {
        ColorService.live ++
        GeneratePictureServiceMock.GeneratePicture(returns = Expectation.failureF(_ => new GenerationError(""))).toLayer
    }

    val layerWithFailedFillPicture =
        ColorService.live ++
            (GeneratePictureServiceMock.GeneratePicture(returns = Expectation.value(Picture(Nil))) ++
            GeneratePictureServiceMock.FillPicture(returns = Expectation.failureF(_ => new GenerationError("")))).toLayer

    override def spec: Spec[TestEnvironment with Scope, Any] = suite("tests") (
        test("firstTask test good") {
            Exercises.task1(1, 1, 2)
                .provideLayer(fullLayer)
                .map(res => assertTrue(res.contains(new Color(1, 1, 2))))
        },
        test("firstTask test fail") {
            Exercises.task1(535, 2424, 24)
                .provideLayer(fullLayer)
                .map(res => assertTrue(res.isEmpty))
        },
        test("second task test") {
            Exercises.task2((4, 4))
                .provideLayer(fullLayer)
                .map(res => assertTrue(res.matches(raw"((\d+\s){3}\d+\n){3}((\d+\s){3}\d+\n?)")))
        },
        test("third task test1") {
            Exercises.task3((4, 4))
                .fold(err => assertTrue(err.getMessage == "Не удалось создать цвет"), _ => assertTrue(false))
                .provideLayer(layerWithFailedRandomColor)
        },
        test("third task test2") {
            Exercises.task3((4, 4))
                .fold(err => assertTrue(err.getMessage == "Ошибка генерации изображения"), _ => assertTrue(false))
                .provideLayer(layerWithFailedGeneratePicture)
        },
        test("third task test3") {
            Exercises.task3((4, 4))
                .fold(err => assertTrue(err.getMessage == "Возникли проблемы при заливке изображения"), _ => assertTrue(false))
                .provideLayer(layerWithFailedFillPicture)
        }
    )
}
