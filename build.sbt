scalaVersion := "3.7.4"

name := "grader"

scalacOptions ++= Seq(
  "-Wall",
  //"-language:noAutoTupling",
  "-Wconf:any:e",
  "-Wunused:all",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-Yexplicit-nulls",
  "-experimental"
)

javaOptions in (Test, run) ++= Seq(
  "-Djdk.tracePinnedThreads"
)

Test / fork := true

testFrameworks += TestFramework("munit.Framework")

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fansi" % "0.5.1",
  "com.lihaoyi" %% "mainargs" % "0.7.7",
  "com.lihaoyi" %% "os-lib" % "0.11.6",
  "com.lihaoyi" %% "pprint" % "0.9.5",
  "com.lihaoyi" %% "sourcecode" % "0.4.4",
  "com.lihaoyi" %% "upickle" % "4.4.1",
  "org.graalvm.polyglot" % "python" % "25.0.1"
)

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.2.1" % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

//dependsOn(RootProject(uri("https://github.com/utgheith/rules.git#e4f1ec414ef661a6f3f2a065eb1a973b9b97b6db")))

enablePlugins(JavaAppPackaging)
