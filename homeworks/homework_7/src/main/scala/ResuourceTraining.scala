package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}

object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] = {
    val acquire = ZIO.attempt(new BufferedReader(new FileReader(filePath)))
    val release = (reader: BufferedReader) => ZIO.attempt(reader.close()).orDie

    ZIO.acquireReleaseWith(acquire)(release) { reader =>
      ZIO.attempt {
        val sb = new StringBuilder
        var line: String = null
        while ({ line = reader.readLine(); line != null }) {
          sb.append(line).append("\n")
        }
        sb.toString()
      }
    }
  }

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] = {
    val acquire = ZIO.attempt(new BufferedWriter(new FileWriter(filePath))).orDie
    val release = (writer: BufferedWriter) => ZIO.attempt(writer.close()).orDie

    ZIO.acquireReleaseWith(acquire)(release) { writer =>
      ZIO.attempt(writer.write(data)).orDie
    }
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}