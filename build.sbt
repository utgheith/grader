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
  "-Yexplicit-nulls",
  "-experimental" // for saferExceptions (ag.k2)
)

Compile / mainClass := Some("Main")
nativeImageVersion := "25.0.2"
nativeImageJvm := "graalvm-community"
//nativeImageOptions += "-H:-UseServiceLoaderFeature"
testFrameworks += TestFramework("munit.Framework")
// the classpath tests compile snippets against (ag.k2.Snippets)
Test / resourceGenerators += Def.task {
  val file = (Test / resourceManaged).value / "snippet-classpath.txt"
  val converter = fileConverter.value
  IO.write(
    file,
    (Test / dependencyClasspath).value
      .map(a => converter.toPath(a.data).toString)
      .mkString(java.io.File.pathSeparator)
  )
  Seq(file)
}.taskValue

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
  "com.lihaoyi" %% "scalatags" % "0.13.1",
  "com.lihaoyi" %% "sourcecode" % "0.4.4",
  "com.lihaoyi" %% "upickle" % "4.4.3"
)

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.3.6" % Test,
  // to compile snippets in tests: some errors (capture checking) come only
  // after the typer, where munit's compileErrors stops
  "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % Test,
  // to find every Event class (ag.grader.EventsTests)
  "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value % Test
)

enablePlugins(NativeImagePlugin)

