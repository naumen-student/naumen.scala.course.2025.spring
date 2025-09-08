ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.16"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.0" % "test"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0"

testFrameworks += new TestFramework("utest.runner.Framework")

lazy val root = (project in file("."))
  .settings(
    name := "homework_5"
  )
