name := "Lecture4"

version := "0.1"

scalaVersion := "2.12.10"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.5" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")