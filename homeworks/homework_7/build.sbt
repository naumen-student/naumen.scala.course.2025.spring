ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "ZioLecture",
    idePackagePrefix := Some("ru.dru"),
    // https://mvnrepository.com/artifact/dev.zio/zio
    libraryDependencies += "dev.zio" %% "zio" % "2.0.5",
      libraryDependencies ++= Seq(
        "dev.zio" %% "zio-test" % "2.0.5" % "test",
        "dev.zio" %% "zio-test-sbt" % "2.0.5" % "test"
      ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
