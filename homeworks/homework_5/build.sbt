version := "0.1"

scalaVersion := "3.3.5"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.5" % "test"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0"

testFrameworks += new TestFramework("utest.runner.Framework")
