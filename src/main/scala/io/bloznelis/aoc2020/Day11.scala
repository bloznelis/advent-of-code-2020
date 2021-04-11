package io.bloznelis.aoc2020

import io.bloznelis.aoc2020.Day11.Tile
import io.bloznelis.aoc2020.util._

import scala.collection.parallel.CollectionConverters._
import scala.util.Try

object Day11 {
  type FloorMap = List[List[Tile]]

  sealed trait Tile {
    val x: Int
    val y: Int
  }
  case class Floor(x: Int, y: Int) extends Tile
  case class Empty(x: Int, y: Int) extends Tile {
    def occupy: Occupied = Occupied(x, y)
  }
  case class Occupied(x: Int, y: Int) extends Tile {
    def free: Tile = Empty(x, y)
  }

  def run() = {
    val input: List[String] = readFileLines("data/input/day11.txt")

    val countOccupied: FloorMap => Int = _.map(_.count(_.isInstanceOf[Occupied])).sum

    val answer1 = countOccupied(go(toMap(input), transform))
    val answer2 = countOccupied(go(toMap(input), transform2))

    (answer1, answer2)
  }

  def toMap(lines: List[String]): List[List[Tile]] =
    lines.zipWithIndex.map { case (str, y) =>
      str.zipWithIndex.map { case (char, x) =>
        char match {
          case '#' => Occupied(x, y)
          case 'L' => Empty(x, y)
          case '.' => Floor(x, y)
        }
      }.toList
    }

  def transform(map: List[List[Tile]]) =
    map.map(row => row.map(tile => transformTile(tile, getAdjecentTiles(tile, map))))

  def getAdjecentTiles(tile: Tile, map: List[List[Tile]]): List[Tile] = {
    val get = (x: Int, y: Int) => map.lift(y).flatMap(_.lift(x))
    val (x, y) = (tile.x, tile.y)
    List(
      get(x + 1, y),
      get(x, y + 1),
      get(x + 1, y + 1),
      get(x - 1, y),
      get(x, y - 1),
      get(x - 1, y - 1),
      get(x + 1, y - 1),
      get(x - 1, y + 1)
    ).flatten
  }

  def transformTile(tile: Tile, adjacent: List[Tile]) = {
    val occupiedAmount = adjacent.count(tile => tile.isInstanceOf[Occupied])

    tile match {
      case empty: Empty if (occupiedAmount == 0) => empty.occupy
      case occupied: Occupied if (occupiedAmount >= 4) => occupied.free
      case other => other
    }
  }

  def transform2(map: List[List[Tile]]) =
    map
      .par
      .map(row => row.map(tile => transformTile2(tile, getVisibleTiles(tile, map))))
      .toList

  def transformTile2(tile: Tile, adjacent: List[Tile]) = {
    val occupiedAmount = adjacent.count(tile => tile.isInstanceOf[Occupied])

    tile match {
      case empty: Empty if (occupiedAmount == 0) => empty.occupy
      case occupied: Occupied if (occupiedAmount >= 5) => occupied.free
      case other => other
    }
  }

  def getVisibleTiles(tile: Tile, map: List[List[Tile]]): List[Tile] = {
    val get = (x: Int, y: Int) => map.lift(y).flatMap(_.lift(x))
    val yRangeNegative =  (-1 to -tile.y by -1).inclusive
    val yRangePositive = (1).until(map.size - tile.y).inclusive
    val xRangeNegative = (-1 to -tile.x by -1).inclusive
    val xRangePositive = (1).until(map.head.size - tile.x).inclusive

    val ZeroRange = 0 until 1

    def scan(range: IndexedSeq[(Int, Int)]): Option[Tile] =
      range
        .flatMap((x,y) => get(tile.x + x, tile.y + y))
        .collectFirst { case o: (Empty | Occupied) if o != tile => o }

    List(
      scan(xRangePositive.zipAll(ZeroRange, 0, 0)),
      scan(ZeroRange.zipAll(yRangePositive, 0, 0)),
      scan(xRangePositive.zip(yRangePositive)),
      scan(xRangeNegative.zipAll(ZeroRange, 0, 0)),
      scan(ZeroRange.zipAll(yRangeNegative, 0,0)),
      scan(xRangeNegative.zip(yRangeNegative)),
      scan(xRangePositive.zip(yRangeNegative)),
      scan(xRangeNegative.zip(yRangePositive))
    ).flatten
  }

  def go(initialMap: FloorMap, fn: FloorMap => FloorMap): FloorMap = {
    val nextMap = transform2(initialMap)
    if (initialMap == nextMap) {
      initialMap
    } else {
      val occupied = nextMap.map(row => row.count(tile => tile.isInstanceOf[Occupied])).sum
      println(s"transforming.. Current occupied amount $occupied")
      go(nextMap, fn)
    }
  }
}
