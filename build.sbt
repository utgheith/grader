scalaVersion := "3.6.2"

scalacOptions ++= Seq(
  "-Wall",
  "-Wconf:any:e",
  //"-Wunused:all",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-Yexplicit-nulls"
)

testFrameworks += TestFramework("munit.Framework")

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fansi" % "0.5.0" ,
  "com.lihaoyi" %% "mainargs" % "0.7.6" ,
  "com.lihaoyi" %% "os-lib" % "0.11.3" ,
  "com.lihaoyi" %% "pprint" % "0.9.0" ,
  "com.lihaoyi" %% "sourcecode" % "0.4.2" ,
  "com.lihaoyi" %% "upickle" % "4.0.2" 
)

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.0.3" % Test,
)

dependsOn(RootProject(uri("https://github.com/utgheith/rules.git")))

enablePlugins(JavaAppPackaging)
