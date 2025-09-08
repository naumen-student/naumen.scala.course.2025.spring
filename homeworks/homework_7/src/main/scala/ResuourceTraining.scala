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
      ZIO.attempt(new BufferedReader(new FileReader(filePath)))
    )(r => ZIO.succeed(r.close()).ignore) {
      r =>
        ZIO.attempt {
          val sb = new StringBuilder
          var l: String = r.readLine()
          while (l != null) {
            sb.append(l)
            l = r.readLine()
            if (l != null)
              sb.append("\n")
          }
          sb.toString
        }
    }

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] =
    ZIO.acquireReleaseWith(
      ZIO.attempt(new BufferedWriter(new FileWriter(filePath))).orDie
    )(w => ZIO.succeed(w.close()).ignore) {
      w => ZIO.attempt(w.write(data)).ignore
    }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}
