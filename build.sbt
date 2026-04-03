scalaVersion := "3.8.3"

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
  "org.graalvm.polyglot" % "python" % "25.0.2"
)

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.2.4" % Test
)

//dependsOn(RootProject(uri("https://github.com/utgheith/rules.git#e4f1ec414ef661a6f3f2a065eb1a973b9b97b6db")))

enablePlugins(JavaAppPackaging)
