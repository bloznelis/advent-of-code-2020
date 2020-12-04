package io.bloznelis.aoc2020

import io.bloznelis.aoc2020.util.{readFileAsString, readFileLines}

import java.nio.file.{Files, Paths}
import scala.util.Try

object Day4 {
  case class Passport(
    byr: String,
    iyr: String,
    eyr: String,
    hgt: String,
    hcl: String,
    ecl: String,
    pid: String,
    cid: Option[String]
  )

  def answer(): Int =
    readFileAsString("data/input/day4.txt")
      .split("\n\n")
      .map(_.replaceAll("\n", " "))
      .map(parsePassport)
      .flatten
      .size

  def parsePassport(line: String): Option[Passport] = {
    def parse(key: String) =
      line.split(" ").map(parseField(_, key)).collectFirst{ case Some(value) => value }

    for {
      byr <- parse("byr")
      if (1920 <= byr.toInt && byr.toInt <= 2002)
      iyr <- parse("iyr")
      if (2010 <= iyr.toInt && iyr.toInt <= 2020)
      eyr <- parse("eyr")
      if (2020 <= eyr.toInt && eyr.toInt <= 2030)
      hgt <- parse("hgt")
      if (isHeightValid(hgt))
      hcl <- parse("hcl")
      if (hcl.matches("^#([a-fA-F0-9]{6})$"))
      ecl <- parse("ecl")
      if (areEyesValid(ecl))
      pid <- parse("pid")
      if (Try(pid.toInt).toOption.isDefined && pid.length == 9)
    } yield Passport(byr, iyr, eyr, hgt, hcl, ecl, pid, parse("cid"))
  }

  def parseField(from: String, key: String): Option[String] =
    from match {
      // Can $key be added as a match condition?
      case s"$_:$value" if from.startsWith(key) => Some(value)
      case _ => None
    }

  def isHeightValid: String => Boolean = {
    case s"${inches}in" => 59 <= inches.toInt && inches.toInt <= 76
    case s"${centimeters}cm" => 150 <= centimeters.toInt && centimeters.toInt <= 193
    case _ => false
  }

  def areEyesValid: String => Boolean = {
    case "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => true
    case _ => false
  }
}