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
    )(br => ZIO.attempt(br.close()).orDie) { br =>
      def readLoop(sb: StringBuilder): IO[Throwable, String] =
        ZIO.attempt(br.readLine()).flatMap {
          case null => ZIO.succeed(sb.toString)
          case line => readLoop(sb.append(line).append("\n"))
        }
      readLoop(new StringBuilder())
    }

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] =
    ZIO.acquireReleaseWith(
      ZIO.attempt(new BufferedWriter(new FileWriter(filePath)))
    )(bw => ZIO.attempt(bw.close()).orDie) { bw =>
      ZIO.attempt(bw.write(data)).orElse(ZIO.unit)
    }.catchAll(_ => ZIO.unit)

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    writeData("test.txt", "Данные для записи в файл.\nНовая строка.") *>
      readData("test.txt").flatMap(content => ZIO.succeed(println(s"Содержимое:\n$content")))
}
