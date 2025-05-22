package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}


/**
 * Необходимо реализовать функции readData и writeData, записывающие и читающие данные в/из файла соответственно.
 * В реализации следует применять безопасное использование ресурсов ZIO.acquireReleaseWith
 */


object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] = {
    ZIO.acquireReleaseWith(ZIO.attempt(new BufferedReader(new FileReader(filePath))))(reader =>
      ZIO.attempt(reader.close()).orDie
    ) { reader =>
      ZIO.attempt {
        val stringBuilder = new StringBuilder
        var str = reader.readLine()
        while (str != null) {
          stringBuilder.append(str)
          str = reader.readLine()
        }
        stringBuilder.toString()
      }.refineToOrDie[Throwable]
    }
  }

  def writeData(filePath: String, data: String): ZIO[Any, Throwable, Unit] = {
    ZIO.acquireReleaseWith(ZIO.attempt(new BufferedWriter(new FileWriter(filePath, false))))(writer => ZIO.attempt(writer.close()).orDie
    ) {
      writer => ZIO.attempt(writer.write(data))
        .flatMap(_ => ZIO.attempt(writer.flush()))
        .refineToOrDie[Throwable]
    }
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}
