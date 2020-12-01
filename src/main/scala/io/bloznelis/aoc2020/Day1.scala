package io.bloznelis.aoc2020

import io.bloznelis.aoc2020.util.readFileLines

import scala.annotation.tailrec

object Day1 {

  def answers(): (Int, Int) = {
    val entries = readFileLines("data/input/day1.txt").map(_.toInt)

    (firstPartAnswer(entries), secondPartAnswer(entries))
  }

  def firstPartAnswer(stars: List[Int]): Int = {
    val indexedStars = stars.zipWithIndex

    @tailrec
    def go(current: (Int, Int), remaining: List[Int]): Int = remaining match {
      case Nil          => go(indexedStars(current._2 + 1), stars)
      case head :: tail => if (head + current._1 == 2020) head * current._1 else go(current, tail)
    }

    go(indexedStars.head, stars)
  }

  /**
   * Not as efficient as first part, but less ugly
   */
  def secondPartAnswer(stars: List[Int]): Int =
    (for {
      star1 <- stars
      star2 <- stars
      star3 <- stars
      answer <- if (star1 + star2 + star3 == 2020) List(star1 * star2 * star3) else List.empty
    } yield answer).head
}
