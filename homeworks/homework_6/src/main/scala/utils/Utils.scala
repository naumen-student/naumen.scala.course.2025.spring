package utils

import zio.{IO, Random, Task, ZIO}

import java.awt.Color

object Utils {
    case class Picture(lines: List[List[Color]])

    class GenerationError(msg: String) extends Throwable {
        override def getMessage: String = msg
    }

    val zeroSize = (0, 0)

}
