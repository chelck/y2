enablePlugins(JavaAppPackaging)

name := "y2"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.2"

//mainClass in (Compile, run) := Some("Main.class")
mainClass in Compile := Some("Main")
