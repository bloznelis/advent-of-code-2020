package io.bloznelis.aoc2020

import io.bloznelis.aoc2020.Day8.Cmd._

import java.util.UUID
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.util.Try

object Day8 {

  sealed trait Cmd {
    val value: Int
    val id: UUID
  }
  object Cmd {
    case class Nop(value: Int, id: UUID) extends Cmd
    case class Jmp(value: Int, id: UUID) extends Cmd
    case class Acc(value: Int, id: UUID) extends Cmd
  }

  enum Result {
    case Infinite(value: Int)
    case Fixed(value: Int)
  }

  def answer: (Result, Result.Fixed) = {
    val lines = util.readFileLines("data/input/day8.txt")

    val brokenCommandSet = parseCommands(lines)
    val commandSets = collectPotentialyFixed(brokenCommandSet)
    val results = commandSets.map(resolve)
    val answer1 = resolve(brokenCommandSet)
    val answer2: Result.Fixed = results.collectFirst{ case fixed@Result.Fixed(_) => fixed }.get

    (answer1, answer2)
  }

  def parseCommands(lines: List[String]): List[Cmd] =
    lines.map {
      case s"nop ${value}" => Nop(value.toInt, UUID.randomUUID())
      case s"jmp ${value}" => Jmp(value.toInt, UUID.randomUUID())
      case s"acc ${value}" => Acc(value.toInt, UUID.randomUUID())
    }

  def collectPotentialyFixed(commads: List[Cmd]): List[List[Cmd]] = {
    val jmpFixed =
      commads
        .collect { case jmp@Jmp(_, _) => commads.indexOf(jmp) }
        .map(jmpIndice => commads.patch(jmpIndice, List(Nop(commads(jmpIndice).value, commads(jmpIndice).id)), 1))
    val nopFixed =
      commads
        .collect { case nop@Nop(_, _) => commads.indexOf(nop) }
        .map(nopIndice => commads.patch(nopIndice, List(Jmp(commads(nopIndice).value, commads(nopIndice).id)), 1))

    jmpFixed ++ nopFixed
  }

  def resolve(commands: List[Cmd]): Result = {
    val indexedCommands = commands.zipWithIndex
    def getNextCmd(idx: Int): Option[(Cmd, Int)] = Try(indexedCommands(idx)).toOption

    def go(acc: Int, cmd: (Cmd, Int), done: HashSet[UUID]): Result = {
      if (done.contains(cmd._1.id))
        Result.Infinite(acc)
      else
        cmd match {
          case (Nop(_, id), idx) => getNextCmd(idx + 1).fold(Result.Fixed(acc))(nextCmd => go(acc, nextCmd, done + cmd._1.id))
          case (Jmp(value, id), idx) => getNextCmd(idx + value).fold(Result.Fixed(acc))(nextCmd => go(acc, nextCmd, done + cmd._1.id))
          case (Acc(value, id), idx) => getNextCmd(idx + 1).fold(Result.Fixed(acc))(nextCmd => go(acc + value, nextCmd, done + cmd._1.id))
        }
    }

    go(0, indexedCommands.head, HashSet.empty)
  }
}