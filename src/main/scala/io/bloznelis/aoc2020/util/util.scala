package io.bloznelis.aoc2020

import java.nio.file.{Files, Paths}

package object util {
  def readFileLines(path: String): List[String] = {
    import scala.jdk.CollectionConverters._

    Files.readAllLines(Paths.get(path)).asScala.toList
  }

  def readFileAsString(path: String): String =
    new String(Files.readAllBytes(Paths.get(path)))
}