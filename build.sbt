//ThisBuild / version := "0.1.0-SNAPSHOT"
//
//ThisBuild / scalaVersion := "3.3.5"
//
//lazy val root = (project in file("."))
//  .settings(
//    name := "homework_2"
//  )
//
//libraryDependencies += "com.lihaoyi" %% "utest" % "0.5.3" % "test"
name := "homework_2"

version := "0.1"

scalaVersion := "2.12.10"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.5.3" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")