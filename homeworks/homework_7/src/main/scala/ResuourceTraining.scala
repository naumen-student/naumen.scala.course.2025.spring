package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}


/**
 * Необходимо реализовать функции readData и writeData, записывающие и читающие данные в/из файла соответственно.
 * В реализации следует применять безопасное использование ресурсов ZIO.acquireReleaseWith
 */


object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] =
    ZIO.acquireReleaseWith(
      ZIO.attempt(new BufferedReader(new FileReader(filePath))) // открытие
    )(reader =>
      ZIO.succeed(reader.close()).ignore          // закрытие (гарантированное)
    ) { reader =>
      ZIO.attempt {
        val content = new StringBuilder
        var line: String = null
        while ({ line = reader.readLine(); line != null }) {
          content.append(line)
        }
        content.toString()
      }
    }

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] =
    ZIO.acquireReleaseWith(
      ZIO.attempt(new BufferedWriter(new FileWriter(filePath))).orDie // открытие
    )(writer =>
      ZIO.succeed(writer.close()).ignore          // закрытие
    ) { writer =>
      ZIO.attempt(writer.write(data)).ignore
    }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}
