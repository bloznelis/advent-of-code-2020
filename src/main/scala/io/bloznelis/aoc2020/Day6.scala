package io.bloznelis.aoc2020

object Day6 {
  val batches = util.readBatches("data/input/day6.txt")
  val first = batches.map(_.replace("\n","").distinct.size).sum
  val second = batches.map(batch => batch.replace("\n","").mkString("").toCharArray.groupMapReduce(identity)(_ => 1)(_ + _).filter((_, amount) => amount == batch.split("\n").size).size).sum
}