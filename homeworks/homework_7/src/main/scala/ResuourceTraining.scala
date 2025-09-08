package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter, IOException}


/**
 * Необходимо реализовать функции readData и writeData, записывающие и читающие данные в/из файла соответственно.
 * В реализации следует применять безопасное использование ресурсов ZIO.acquireReleaseWith
 */


object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] = {
    def openFile(name: => String): ZIO[Any, IOException, FileReader] =
      ZIO.attemptBlockingIO(new FileReader(name))

    def closeFile(source: => FileReader): ZIO[Any, Nothing, Unit] =
      ZIO.succeedBlocking(source.close())

    def read(source: => FileReader): ZIO[Any, Throwable, String] =
      ZIO.attemptBlocking(new BufferedReader(source).lines.toArray().mkString)
    for {
      x <- ZIO.acquireReleaseWith(openFile(filePath))(closeFile(_))(read(_))
    } yield x
  }

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] = {
    def openFile(name: String): ZIO[Any, IOException, FileWriter] =
      ZIO.attemptBlockingIO(new FileWriter(name))

    def closeFile(source: FileWriter): ZIO[Any, Nothing, Unit] =
      ZIO.succeedBlocking(source.close())

    def write(source: FileWriter): ZIO[Any, Throwable, Unit] = {
      ZIO.attemptBlockingIO {
        val writer = new BufferedWriter(source)
        writer.write(data)
        writer.flush()
      }
    }
    ZIO.acquireReleaseWith(openFile(filePath))(closeFile)(write)
      .catchAll(_ => ZIO.unit)
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}
