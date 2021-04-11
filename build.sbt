lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2020",
    description := "Advent of Code in Scala 3",
    version := "0.1.0",
    scalaVersion := "3.0.0-RC2",
      libraryDependencies ++= Seq(
          "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.2"
      )
  )
