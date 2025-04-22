ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.10"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.5.3" % "test"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0"

testFrameworks += new TestFramework("utest.runner.Framework")

lazy val root = (project in file("."))
  .settings(
    name := "homework_5"
  )
