package io.bloznelis.aoc2020

import io.bloznelis.aoc2020.util.readFileLines

import scala.annotation.tailrec

object Day1 {

  def answers(): (Int, Int) = {
    val entries = readFileLines("data/input/day1.txt").map(_.toInt)

    (firstPartAnswer(entries), secondPartAnswer(entries))
  }

  def firstPartAnswer(entries: List[Int]): Int = {
    val indexedEntries = entries.zipWithIndex

    @tailrec
    def go(current: (Int, Int), remaining: List[Int]): Int = remaining match {
      case Nil          => go(indexedEntries(current._2 + 1), entries)
      case head :: tail => if (head + current._1 == 2020) head * current._1 else go(current, tail)
    }

    go(indexedEntries.head, entries)
  }

  /**
   * Not as efficient as first part, but less ugly
   */
  def secondPartAnswer(entries: List[Int]): Int =
    (for {
      entry1 <- entries
      entry2 <- entries
      entry3 <- entries
      answer <- if (entry1 + entry2 + entry3 == 2020) List(entry1 * entry2 * entry3) else List.empty
    } yield answer).head
}