package ru.dru

import ru.dru.BreakfastTest.suite
import zio.Scope
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue}
import zio._

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}

object ResuourceTrainingTest extends ZIOSpecDefault {

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("ResuourceTraining") (
    test("test reader 1") {
      for {
        _ <- ZIO.acquireReleaseWith(ZIO.succeed(
          new BufferedWriter(new FileWriter("in.txt", false))))(writer =>
          ZIO.succeed(writer.close())
        ){ writer =>
          ZIO.succeed(writer.write("Hello, world!")).map(_ => writer.flush())
        }

        data <- ResuourceTraining.readData("in.txt")
      } yield {
        assertTrue(data == "Hello, world!")
      }
    },


    test("test reader 2") {
      for {
        _ <- ZIO.acquireReleaseWith(ZIO.succeed(
          new BufferedWriter(new FileWriter("in2.txt", false))))(writer =>
          ZIO.succeed(writer.close())
        ) { writer =>
          ZIO.succeed(writer.write("Hello, world! Byeeee!")).map(_ => writer.flush())
        }

        data <- ResuourceTraining.readData("in2.txt")
      } yield {
        assertTrue(data == "Hello, world! Byeeee!")
      }
    },



    test("writer1") {
      for {

        _ <- ResuourceTraining.writeData("out.txt", "Hello, world")

        data <- ZIO.acquireReleaseWith(ZIO.succeed(
          new BufferedReader(new FileReader("out.txt"))))(reader =>
          ZIO.succeed(reader.close())
        ) { reader =>
          ZIO.succeed(reader.readLine())
        }


      } yield {
        assertTrue(data == "Hello, world")
      }
    },

    test("writer2") {
      for {

        _ <- ResuourceTraining.writeData("out2.txt", "Hello, world! My name is Vasya")

        data <- ZIO.acquireReleaseWith(ZIO.succeed(
          new BufferedReader(new FileReader("out2.txt"))))(reader =>
          ZIO.succeed(reader.close())
        ) { reader =>
          ZIO.succeed(reader.readLine())
        }


      } yield {
        assertTrue(data == "Hello, world! My name is Vasya")
      }
    }
  )
}
