scalaVersion := "3.10.0"


name := "grader"

scalacOptions ++= Seq(
  "-Wall",
  "-Werror",
  "-Wconf:any:e",
  // "-Wunused:all",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-Yexplicit-nulls"
  //"-experimental"
)

Compile / mainClass := Some("Main")
nativeImageVersion := "25.0.2"
nativeImageJvm := "graalvm-community"
//nativeImageOptions += "-H:-UseServiceLoaderFeature"
testFrameworks += TestFramework("munit.Framework")
// in a JVM of their own, so their temporary directories (TempDirs) are
// removed when it exits: sbt's own JVM unloads the tests' classes first
Test / fork := true

nativeImageOptions ++= Seq(
      "--no-fallback",
      "-H:+ReportExceptionStackTraces",
      "--initialize-at-build-time=scala.runtime.Statics$VM"
)

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fansi" % "0.5.1",
  "com.lihaoyi" %% "mainargs" % "0.7.8",
  "com.lihaoyi" %% "os-lib" % "0.11.10",
  "com.lihaoyi" %% "pprint" % "0.9.6",
  "com.lihaoyi" %% "scalatags" % "0.13.1",
  "com.lihaoyi" %% "sourcecode" % "0.4.4",
  "com.lihaoyi" %% "upickle" % "4.4.3",
  // ag.r3 keeps its targets in a git repo
  "org.eclipse.jgit" % "org.eclipse.jgit" % "7.8.0.202609011348-r",
  "org.slf4j" % "slf4j-nop" % "2.0.17"
)

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.3.6" % Test
)

enablePlugins(NativeImagePlugin)

