lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2020",
    description := "Advent of Code in Scala 3",
    version := "0.1.0",
    scalaVersion := "3.0.0-M2",
    useScala3doc := true
  )
