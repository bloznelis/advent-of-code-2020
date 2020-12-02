package io.bloznelis.aoc2020

import io.bloznelis.aoc2020.util.readFileLines
import scala.util.Try

object Day2 {
  case class Validation(char: Char, range: Range)
  case class Password(validFn: Validation, passwordLiteral: String) {
    lazy val isValid1: Boolean =
      validFn
        .range
        .contains(passwordLiteral.toCharArray.groupMapReduce(identity)(_ => 1)(_ + _).getOrElse(validFn.char, 0))

    lazy val isValid2: Boolean = {
      val chars = passwordLiteral.toCharArray.toList
      chars(validFn.range.head - 1) == validFn.char ^ chars(validFn.range.last - 1) == validFn.char
    }
  }

  def answers(): (Int, Int) = {
    val passwords = readFileLines("data/input/day2.txt").map(read)
    val getAnswer: (Password => Boolean) => Int = passwords.filter(_).size

    (getAnswer(_.isValid1), getAnswer(_.isValid2))
  }

  def read: String => Password =
    case s"${lower}-${upper} ${char}: ${password}" => Password(Validation(char.toCharArray.head, (lower.toInt until upper.toInt).inclusive), password)
}