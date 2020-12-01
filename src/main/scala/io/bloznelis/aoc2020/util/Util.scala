package io.bloznelis.aoc2020.util

import java.nio.file.{Files, Paths}

object Util {

  def readFileLines(path: String): List[String] = {
    import scala.jdk.CollectionConverters._

    Files.readAllLines(Paths.get(path)).asScala.toList
  }
}
