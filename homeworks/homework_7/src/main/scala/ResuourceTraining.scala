package ru.dru

import zio._

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}





object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] =
    ZIO.acquireReleaseWith(
      ZIO.attempt(new BufferedReader(new FileReader(filePath)))
    )(reader => ZIO.succeed(reader.close()).ignore) { reader =>
      ZIO.attempt {
        Iterator.continually(reader.readLine()).takeWhile(_ != null).mkString("\n")
      }
    }

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] =
    ZIO.acquireReleaseWith(
      ZIO.attempt(new BufferedWriter(new FileWriter(filePath))).catchAll(_ => ZIO.succeed(null))
    )(writer => ZIO.succeed(if (writer != null) writer.close()).ignore) { writer =>
      ZIO.attempt {
        if (writer != null) {
          writer.write(data)
          writer.flush()
        }
      }.catchAll(_ => ZIO.unit)
    }.unit
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}
