scalaVersion := "3.9.0"


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

nativeImageOptions ++= Seq(
      "--no-fallback",
      "-H:+ReportExceptionStackTraces",
      "--initialize-at-build-time=scala.runtime.Statics$VM"
)

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fansi" % "0.5.1",
  "com.lihaoyi" %% "mainargs" % "0.7.8",
  "com.lihaoyi" %% "os-lib" % "0.11.8",
  "com.lihaoyi" %% "pprint" % "0.9.6",
  "com.lihaoyi" %% "sourcecode" % "0.4.4",
  "com.lihaoyi" %% "upickle" % "4.4.3"
)

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.3.5" % Test
)

enablePlugins(NativeImagePlugin)

