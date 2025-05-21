package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}


/**
 * Необходимо реализовать функции readData и writeData, записывающие и читающие данные в/из файла соответственно.
 * В реализации следует применять безопасное использование ресурсов ZIO.acquireReleaseWith
 */


object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] = {
    ZIO.acquireReleaseWith(ZIO.attempt(new BufferedReader(new FileReader(filePath))))(r =>
      ZIO.attempt(r.close()).orDie)(r => ZIO.attempt(r.readLine()))
  }

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] = {
    ZIO.acquireReleaseWith(ZIO.attempt(new BufferedWriter(new FileWriter(filePath))))(w =>
      ZIO.attempt(w.close()).orDie)(w => ZIO.attempt {
      w.write(data)
      w.flush()
      }).orDie
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}
