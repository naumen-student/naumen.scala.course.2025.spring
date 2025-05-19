ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "homework_6",
      libraryDependencies ++= Seq(
          "dev.zio" %% "zio" % "2.0.19",
          "dev.zio" %% "zio-test" % "2.0.19" % Test,
          "dev.zio" %% "zio-test-sbt" % "2.0.19" % "test",
          "dev.zio" %% "zio-mock" % "1.0.0-RC12",
          "eu.timepit" %% "refined" % "0.9.27"
      ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
