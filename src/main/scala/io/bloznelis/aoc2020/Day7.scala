package io.bloznelis.aoc2020

import scala.collection.immutable.HashSet
import scala.collection.mutable

object Day7 {
  case class Rule(name: String, containables: Vector[(String, Int)]) {
    def canContain(name: String): Boolean = containables.find(tpl => tpl._1 == name).isDefined
  }

  def answers: (Int, Int) = {
    val rules = util.readFileLines("data/input/day7.txt").map(parseRule)

    val answer1 = countParents(rules.toList)
    val answer2 = countContainingBagAmount(rules)

    (answer1, answer2)
  }

  def parseRule(line: String): Rule = {
    line match {
      case s"${bagName} bags contain no other bags." => Rule(bagName, Vector.empty)
      case _ =>
        val commaSplit = line.split(",")
        val name = commaSplit.head.split(" ").take(2).mkString(" ")
        val contains1 = commaSplit.head.split(" ").drop(4)
        val contains2: (String, Int) = (s"${contains1(1)} ${contains1(2)}", contains1.head.toInt)

        val remaining =
          commaSplit.drop(1).map{containable =>
            val split = containable.trim.split(" ")
            (s"${split(1)} ${split(2)}") -> split.head.toInt
          }

        Rule(name, contains2 +: remaining.toVector)
    }
  }

  def countParents(rules: List[Rule]): Int = {
    def findParents(name: String, currentParents: List[Rule]): List[Rule] = {
      val parents = rules.collect{case possibleParent if possibleParent.canContain(name) => possibleParent }
      parents match {
        case Nil => currentParents
        case rules => rules.flatMap(rule => findParents(rule.name, currentParents ++ rules))
      }
    }

    findParents("shiny gold", List.empty).distinct.size
  }

  def countContainingBagAmount(rules: List[Rule]): Int = {
    val rulesAsMap = rules.map(r => r.name -> r.containables.toList).toMap
    def go(name:String): Int = rulesAsMap(name) match {
      case Nil => 0
      case containables =>
        containables
          .map((containableName, containableSize) => containableSize + containableSize * go(containableName))
          .sum
    }

    go("shiny gold")
  }
}
