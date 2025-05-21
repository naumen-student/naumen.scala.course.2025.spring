import utils.{ColorService, PictureGenerationService, Utils}
import utils.ColorService.ColorService
import utils.PictureGenerationService.PictureGenerationService
import utils.Utils.GenerationError
import zio.{IO, Random, Task, URLayer, ZIO, ZLayer, mock}
import zio.mock.Mock

import java.awt.Color

object Mocks {

    object ColorServiceMock extends Mock[ColorService.Service] {
        object GenerateRandomColor extends Effect[Unit, Utils.GenerationError, Color]

        override val compose: URLayer[mock.Proxy, ColorService.Service] =
            ZLayer {
                for {
                    proxy <- ZIO.service[mock.Proxy]
                } yield new ColorService {
                    override def generateRandomColor(): IO[Utils.GenerationError, Color] =
                        proxy(GenerateRandomColor)

                    override def getColor(r: Int, g: Int, b: Int): Task[Color] =
                        ZIO.succeed(new Color(r, g, b))
                }
            }
    }

    object GeneratePictureServiceMock extends Mock[PictureGenerationService.Service] {
        object GeneratePicture extends Effect[Unit, Utils.GenerationError, Utils.Picture]

        object FillPicture extends Effect[Unit, Utils.GenerationError, Utils.Picture]

        override val compose: URLayer[mock.Proxy, PictureGenerationService.Service] = ZLayer {
            for {
                proxy <- ZIO.service[mock.Proxy]
            } yield new PictureGenerationService.Service {
                override def generatePicture(size: (Int, Int)): IO[GenerationError, Utils.Picture] =
                    proxy(GeneratePicture)

                override def fillPicture(picture: Utils.Picture, color: Color): Task[Utils.Picture] =
                    proxy(FillPicture)
            }
        }
    }

}
