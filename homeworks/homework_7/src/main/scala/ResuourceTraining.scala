package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}


/**
 * Необходимо реализовать функции readData и writeData, записывающие и читающие данные в/из файла соответственно.
 * В реализации следует применять безопасное использование ресурсов ZIO.acquireReleaseWith
 */


object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] = {
    def readFile(reader: BufferedReader) = {
      ZIO.attempt(reader.readLine())
    }
    def releaseReader(reader: BufferedReader) = {
      ZIO.succeed(reader.close())
    }
    def acquireReader(filePath: String) = {
      ZIO.attempt(new BufferedReader(new FileReader(filePath), 2048))
    }
    ZIO.acquireReleaseWith(acquireReader(filePath))(releaseReader)(readFile)
  }

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] = {
    def writeFile(writer: FileWriter) = {
      ZIO.succeed(writer.write(data))
    }
    def releaseWriter(writer: FileWriter) = {
      ZIO.succeed(writer.close())
    }
    def acquireWriter(filePath: String) = {
      ZIO.succeed(new FileWriter(filePath))
    }
    ZIO.acquireReleaseWith(acquireWriter(filePath))(releaseWriter)(writeFile)
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}
