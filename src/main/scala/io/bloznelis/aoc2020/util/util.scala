package io.bloznelis.aoc2020

import java.nio.file.{Files, Paths}

package object util {
  def readFileLines(path: String): List[String] = {
    import scala.jdk.CollectionConverters._

    Files.readAllLines(Paths.get(path)).asScala.toList
  }
}
