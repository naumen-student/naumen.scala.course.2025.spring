package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}


/**
 * Необходимо реализовать функции readData и writeData, записывающие и читающие данные в/из файла соответственно.
 * В реализации следует применять безопасное использование ресурсов ZIO.acquireReleaseWith
 */


object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] = {
    ZIO.acquireReleaseWith(
      ZIO.attempt(new BufferedReader(new FileReader(filePath)))
    )(
      reader => ZIO.attempt(reader.close()).orDie
    ) { reader =>
      ZIO.attempt {
        val content = new StringBuilder()
        var line = reader.readLine()
        while (line != null) {
          content.append(line).append("\n")
          line = reader.readLine()
        }
        content.toString()
      }
    }
  }

  def writeData(filePath: String, data: String): IO[Throwable, Unit] = {
    ZIO.acquireReleaseWith(
      ZIO.attemptBlocking(new BufferedWriter(new FileWriter(filePath)))
    )(
      writer => ZIO.attemptBlocking(writer.close()).orDie
    ) { writer =>
      ZIO.attemptBlocking(writer.write(data))
    }
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}
