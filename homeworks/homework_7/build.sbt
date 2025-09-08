ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.5"

lazy val root = (project in file("."))
  .settings(
    name := "ZioLecture",
    idePackagePrefix := Some("ru.dru"),
    // https://mvnrepository.com/artifact/dev.zio/zio
    libraryDependencies += "dev.zio" %% "zio" % "2.1.18",
      libraryDependencies ++= Seq( "dev.zio" %% "zio-test" % "2.1.18" % Test )
  )
