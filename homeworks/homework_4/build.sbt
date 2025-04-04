//ThisBuild / version := "0.1.0-SNAPSHOT"
//
//ThisBuild / scalaVersion := "3.3.5"
//libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.1" % "test"
//testFrameworks += new TestFramework("utest.runner.Framework")
//lazy val root = (project in file("."))
//  .settings(
//    name := "homework_4"
//  )
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.10"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.5.3" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")

lazy val root = (project in file("."))
  .settings(
    name := "homework-4"
  )