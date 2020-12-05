package io.bloznelis.aoc2020

import io.bloznelis.aoc2020.util.{readFileAsString, readFileLines}

import java.nio.file.{Files, Paths}
import scala.util.Try

object Day5 {
  case class Instructions(row: Vector[Instruction], column: Vector[Instruction])
  enum Instruction {
    case Upper
    case Lower
  }

  def answers: (Int, Int) = {
    val ids = readFileLines("data/input/day5.txt").map(parse andThen findSeatID)
    val answer1 = ids.max
    val answer2 = ids.sorted.sliding(2, 1).map(ls => if (ls(1) - ls(0) != 1) Some((ls(0) + 1)) else None).flatten.toVector.head

    (answer1, answer2)
  }
  def findSeatID(instructions: Instructions): Int =
    binarySearch(instructions.row, 128) * 8 + binarySearch(instructions.column, 8)

  def parse(line: String): Instructions = Instructions.tupled(
    line
      .toCharArray
      .map {
        case 'F' => Instruction.Lower
        case 'B' => Instruction.Upper
        case 'L' => Instruction.Lower
        case 'R' => Instruction.Upper
      }
      .toVector
      .splitAt(7)
  )

  def binarySearch(instructions: Vector[Instruction], upper: Int): Int =
    instructions.foldLeft((0, upper)){ (current, instruction) =>
      val half = current._1 + (current._2 - current._1) / 2
      instruction match {
        case Instruction.Upper => (half, current._2)
        case Instruction.Lower => (current._1, half)
      }
    }._1
}