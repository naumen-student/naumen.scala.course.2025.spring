package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}


/**
 * Необходимо реализовать функции readData и writeData, записывающие и читающие данные в/из файла соответственно.
 * В реализации следует применять безопасное использование ресурсов ZIO.acquireReleaseWith
 */


object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] = {
    val acquire = ZIO.attempt(new BufferedReader(new FileReader(filePath)))
    val release = (r: BufferedReader) => ZIO.attempt(r.close()).orDie
    val use = (r: BufferedReader) => ZIO.attempt {
      val content = new StringBuilder
      var line = r.readLine()
      while (line != null) {
        content.append(line).append("\n")
        line = r.readLine()
      }
      content.toString()
    }

    ZIO.acquireReleaseWith(acquire)(release)(use)
  }

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] = {
    val acquireResource = ZIO.attempt(new BufferedWriter(new FileWriter(filePath)))
    val releaseResource = (w: BufferedWriter) => ZIO.attempt(w.close()).orDie
    val useResource = (w: BufferedWriter) => ZIO.attempt {
      w.write(data)
      w.newLine()
      w.flush()
    }

    ZIO.acquireReleaseWith(acquireResource)(releaseResource)(useResource).orDie
  }
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}
