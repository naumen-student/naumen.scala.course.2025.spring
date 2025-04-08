version := "0.1"

scalaVersion := "3.3.5"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.5" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")

lazy val root = (project in file("."))
  .settings(
    name := "homework-1"
  )
