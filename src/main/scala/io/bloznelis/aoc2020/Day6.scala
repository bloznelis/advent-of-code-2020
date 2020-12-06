package io.bloznelis.aoc2020

import io.bloznelis.aoc2020.util.{readBatches, readFileAsString}

object Day6 {
  val batches = readBatches("data/input/day6.txt")
  val first = batches.map(_.replace("\n","").distinct.size).sum
  val second = batches.map(batch => batch.replace("\n","").mkString("").toCharArray.groupMapReduce(identity)(_ => 1)(_ + _).filter((_, amount) => amount == batch.split("\n").size).size).sum
}