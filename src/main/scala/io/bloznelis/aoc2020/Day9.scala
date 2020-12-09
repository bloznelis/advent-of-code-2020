package io.bloznelis.aoc2020

import io.bloznelis.aoc2020.Day8.Cmd._

import java.util.UUID
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.util.Try

object Day9 {
  def answer: (Long, Long) = {
    val numbers = util.readFileLines("data/input/day9.txt").map(_.toLong)
    val answer1 =
      numbers
        .sliding(26, 1)
        .flatMap(slice => Option.unless(validate(slice.last, slice))(slice.last))
        .toSeq
        .head

    val sum = findContiguousSet(answer1, numbers)
    val answer2 = sum.min + sum.max

    (answer1, answer2)
  }

  def validate(number: Long, preamble: List[Long]): Boolean =
    (for ( x <- preamble; y <- preamble ) yield (x != y) && x + y == number).reduce(_ || _)

  def findContiguousSet(target: Long, numbers: List[Long]): List[Long] =
    (for {
      i     <- 2 until 1000
      slice <- numbers.sliding(i, 1)
    } yield slice).collectFirst{ case ls if ls.sum == target => ls }.head
}