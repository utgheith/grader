scalaVersion := "3.8.4"

name := "grader"

scalacOptions ++= Seq(
  "-Wall",
  "-Werror",
  "-Wconf:any:e",
  // "-Wunused:all",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-Yexplicit-nulls",
  "-experimental"
)

testFrameworks += TestFramework("munit.Framework")

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fansi" % "0.5.1",
  "com.lihaoyi" %% "mainargs" % "0.7.8",
  "com.lihaoyi" %% "os-lib" % "0.11.8",
  "com.lihaoyi" %% "pprint" % "0.9.6",
  "com.lihaoyi" %% "sourcecode" % "0.4.4",
  "com.lihaoyi" %% "upickle" % "4.4.3",
  "org.graalvm.polyglot" % "python" % "25.2.4"
)

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.3.5" % Test
)

