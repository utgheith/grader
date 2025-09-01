scalaVersion := "3.7.2"

name := "grader"

scalacOptions ++= Seq(
  "-Wall",
  "-Wconf:any:e",
  //"-Wunused:all",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-Yexplicit-nulls",
  "-experimental"
)

testFrameworks += TestFramework("munit.Framework")

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fansi" % "0.5.1" ,
  "com.lihaoyi" %% "mainargs" % "0.7.6" ,
  "com.lihaoyi" %% "os-lib" % "0.11.5" ,
  "com.lihaoyi" %% "pprint" % "0.9.3" ,
  "com.lihaoyi" %% "sourcecode" % "0.4.4" ,
  "com.lihaoyi" %% "upickle" % "4.3.0",
  //"org.graalvm.polyglot" % "python" % "24.1.1"
)

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.1.1" % Test,
)

//dependsOn(RootProject(uri("https://github.com/utgheith/rules.git#e4f1ec414ef661a6f3f2a065eb1a973b9b97b6db")))

enablePlugins(JavaAppPackaging)

