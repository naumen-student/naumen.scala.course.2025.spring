object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] = ???

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] = ???
  def readData(filePath: String): IO[Throwable, String] = {
    ZIO.acquireReleaseWith(
      ZIO.attempt(new BufferedReader(new FileReader(filePath))))(reader => ZIO.succeed(reader.close())) {
      reader => ZIO.attempt(reader.readLine())
    }
  }

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] = {
    ZIO.acquireReleaseWith(
      ZIO.succeed(new FileWriter(filePath)))(writer => ZIO.succeed(writer.close())) {
      writer => ZIO.succeed(writer.write(data))
    }
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}
