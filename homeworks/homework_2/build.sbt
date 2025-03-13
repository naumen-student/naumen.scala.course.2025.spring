name := "Lecture2"

version := "17.0.12"

scalaVersion := "2.13.16"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.6.9" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")