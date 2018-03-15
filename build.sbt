name := "algorithms-clrs"

version := "0.1"

scalaVersion := "2.12.4"

mainClass in (Compile, run) := Some("com.taintech.algorithms.Main")

val scalaTestVersion = "3.0.5"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test
)
