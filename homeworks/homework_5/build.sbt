ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.5"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.5" % "test"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0"

testFrameworks += new TestFramework("utest.runner.Framework")

lazy val root = (project in file("."))
  .settings(
    name := "homework_5"
  )
