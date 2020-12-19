package io.bloznelis.aoc2020

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.{AbstractMap, SeqMap, SortedMap}
import scala.collection.parallel.CollectionConverters._

object Day10 {

  def answers: (Int, Long) = {
    val lines = util.readFileLines("data/input/day10.txt")
    val adapterRatings = lines.map(_.toInt)
    val deviceRating = adapterRatings.max + 3

    val sorted = (Vector(deviceRating, 0) ++ adapterRatings).sorted

    val slided = sorted.sliding(2).map(ls => (ls.head, ls.last)).toVector

    val diffs = slided.foldLeft(Map.empty[Int, Int]){ (acc, pair) =>
      acc.updatedWith(pair._2 - pair._1)(some => Option(some.getOrElse(0) + 1))
    }

    val answer1 = diffs(1) + diffs(3)
    val answer2 = numWays(sorted.toList, deviceRating)

    (answer1, answer2)
  }

  def numWays(adapters: List[Int], maxVal: Int): Long = {
    // XXX: sneaky mutable cache
    var cache: Map[Int, Long] = Map.empty

    def go(currentAdapter: Int): Long = {
      cache.get(currentAdapter).getOrElse {
        val res =
          if (currentAdapter == maxVal)
            1
          else {
            val currentIdx = adapters.indexOf(currentAdapter)
            val next3Valid = adapters.splitAt(currentIdx)._2.filter(_ - adapters(currentIdx) <= 3)
            def traverse: Int => Long = next3Valid.drop(_).headOption.map(nextAdapter => go(nextAdapter)).getOrElse(0L)

            traverse(1) + traverse(2) + traverse(3)
          }

        cache = cache.updated(currentAdapter, res)

        res
      }
    }

    go(0)
  }
}