ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.16"

lazy val root = (project in file("."))
  .settings(
    name := "homework_4"
  )

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.5" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")

