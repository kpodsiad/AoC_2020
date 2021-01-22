val dottyVersion = "3.0.0-M3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc",
    version := "0.1.0",

    scalaVersion := dottyVersion,

  )
