name := "Lecture2"

version := "0.1"

scalaVersion := "2.13.14"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.3" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")