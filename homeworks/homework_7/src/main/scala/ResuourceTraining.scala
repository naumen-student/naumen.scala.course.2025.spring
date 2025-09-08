package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}
import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}

object ResourceTraining extends ZIOAppDefault {

  private def acquireReader(filePath: String): IO[Throwable, BufferedReader] =
    ZIO.attempt(new BufferedReader(new FileReader(filePath)))

  private def acquireWriter(filePath: String): IO[Throwable, BufferedWriter] =
    ZIO.attempt(new BufferedWriter(new FileWriter(filePath, false)))

  private def releaseReader(reader: BufferedReader): IO[Nothing, Unit] =
    ZIO.attempt(reader.close()).orDie

  private def releaseWriter(writer: BufferedWriter): IO[Nothing, Unit] =
    ZIO.attempt(writer.close()).orDie

  def readData(filePath: String): IO[Throwable, String] = {
    ZIO.acquireReleaseWith(acquireReader(filePath))(releaseReader) { reader =>
      ZIO.attempt {
        val sb = new StringBuilder
        var line = reader.readLine()
        while (line != null) {
          sb.append(line)
          line = reader.readLine()
        }
        sb.toString()
      }.refineToOrDie[Throwable]
    }
  }

  def writeData(filePath: String, data: String): IO[Throwable, Unit] = {
    ZIO.acquireReleaseWith(acquireWriter(filePath))(releaseWriter) { writer =>
      ZIO.attempt(writer.write(data)) *>
        ZIO.attempt(writer.flush())
    }.refineToOrDie[Throwable]
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    ZIO.succeed("Done")
}