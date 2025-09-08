ThisBuild / scalaVersion     := "3.3.5"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "homework_6",
      libraryDependencies ++= Seq(
          "dev.zio" %% "zio" % "2.1.18",
          "dev.zio" %% "zio-test" % "2.1.18" % Test,
          "dev.zio" %% "zio-test-sbt" % "2.1.18" % "test",
          "dev.zio" %% "zio-mock" % "1.0.0-RC12",
          "eu.timepit" %% "refined" % "0.11.3"
      ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
