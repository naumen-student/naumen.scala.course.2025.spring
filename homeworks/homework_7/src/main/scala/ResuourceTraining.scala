object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] = ???

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] = ???
  def readData(filePath: String): IO[Throwable, String] = {
    ZIO.acquireReleaseWith(ZIO.attempt(new BufferedReader(new FileReader(filePath))))
      .apply(f => ZIO.attempt(f.close()).orDie)
      .apply(f => ZIO.attempt(f.readLine()))
  }

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] = {
    ZIO.acquireReleaseWith(ZIO.attempt(new BufferedWriter(new FileWriter(filePath))))
      .apply(f => ZIO.attempt(f.close()).orDie)
      .apply(f => ZIO.attempt {
        f.write(data)
        f.flush()
      })
      .orDie
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}
