name := "Lecture5"

version := "0.1"

scalaVersion := "2.13.14"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.3" % "test"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0"

testFrameworks += new TestFramework("utest.runner.Framework")