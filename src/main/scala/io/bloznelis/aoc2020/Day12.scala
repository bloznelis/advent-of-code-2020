package io.bloznelis.aoc2020

import io.bloznelis.aoc2020.Day12.State.turnWaypoint

object Day12 extends App {

  enum MoveDirection {
    case North(value: Int)
    case South(value: Int)
    case West(value: Int)
    case East(value: Int)
    case Left(value: Int)
    case Right(value: Int)
    case Forward(value: Int)
  }
  
  enum Direction(val angle: Int) {
    case East extends Direction(0)
    case North extends Direction(90)
    case West extends Direction(180)
    case South extends Direction(270)
  }
  
  case class ShipState(x: Int, y: Int, facingDirection: Direction) {
    val move: MoveDirection => ShipState =
      case MoveDirection.North(value) => ShipState(x, y + value, facingDirection)
      case MoveDirection.South(value) => ShipState(x, y - value, facingDirection)
      case MoveDirection.West(value) => ShipState(x - value, y, facingDirection)
      case MoveDirection.East(value) => ShipState(x + value, y, facingDirection)
      case MoveDirection.Left(value) => ShipState(x, y, turn(value, facingDirection))
      case MoveDirection.Right(value) => ShipState(x, y, turn(-value, facingDirection))
      case MoveDirection.Forward(value) => facingDirection match {
        case Direction.East => ShipState(x + value, y, facingDirection) 
        case Direction.North => ShipState(x, y + value, facingDirection)
        case Direction.South => ShipState(x, y - value, facingDirection)
        case Direction.West => ShipState(x - value, y, facingDirection)
    }
  }
  
  object ShipState {
    val move: (ShipState, MoveDirection) => ShipState = (state, moveDirection) => state.move(moveDirection)
  }
  
  
  case class Ship(x: Long, y: Long) {
    def moveToWaypoint(waypoint: Waypoint): Ship = Ship(waypoint.x, waypoint.y)
  }
  case class Waypoint(x: Long, y: Long)
  case class State(ship: Ship, waypoint: Waypoint) { self =>
    def moveShipToWaypoint: State = {
      val (relativeX, relativeY) = (waypoint.x - ship.x, waypoint.y - ship.y)
      
      State(ship.moveToWaypoint(waypoint), Waypoint(waypoint.x + relativeX, waypoint.y + relativeY))
    }
    
    val move: MoveDirection => State =
      case MoveDirection.North(value) => State(ship, Waypoint(waypoint.x, waypoint.y + value))
      case MoveDirection.South(value) => State(ship, Waypoint(waypoint.x, waypoint.y - value))
      case MoveDirection.West(value) => State(ship, Waypoint(waypoint.x - value, waypoint.y))
      case MoveDirection.East(value) => State(ship, Waypoint(waypoint.x + value, waypoint.y))
      case MoveDirection.Left(value) => State(ship, turnWaypoint(ship, waypoint, value))
      case MoveDirection.Right(value) => State(ship, turnWaypoint(ship, waypoint, -value))
      case MoveDirection.Forward(value) => State.moveToWaypoint(self, value)
  }
  
  object State {
    val move: (State, MoveDirection) => State = (state, moveDirection) => state.move(moveDirection)
    
    def turnWaypoint(ship: Ship, currentWaypoint: Waypoint, turnValue: Int) = {
      
      val timesToTurn = Math.abs(turnValue / 90)
      
      def turn(fn: Waypoint => Waypoint): Waypoint => Waypoint =
        (0 until timesToTurn).map(_ => fn).reduce(_ andThen _)
      
      val performTurns =
        if (turnValue > 0)
          turn(singleTurnLeft(_, ship))
        else
          turn(singleTurnRight(_, ship))

      performTurns(currentWaypoint)
    }
    
    def singleTurnLeft(currentWaypoint: Waypoint, origin: Ship): Waypoint =
      val (relativeX, relativeY) = (currentWaypoint.x - origin.x, currentWaypoint.y - origin.y)
      Waypoint(origin.x - relativeY, origin.y + relativeX)

    def singleTurnRight(currentWaypoint: Waypoint, origin: Ship): Waypoint =
      val (relativeX, relativeY) = (currentWaypoint.x - origin.x, currentWaypoint.y - origin.y)
      Waypoint(origin.x + relativeY, origin.y - relativeX)
      
    def moveToWaypoint(state: State, times: Int): State = {
      (0 until times).map(_ => (state: State) => state.moveShipToWaypoint).reduce(_ andThen _)(state)
    }

  }
  
  def turn(turnValue: Int, currentDirection: Direction): Direction = {
    def currentAngle = currentDirection.angle
    
    val newAngle = currentAngle + turnValue
    
    
    val normalisedAngle =
      if (newAngle < 0) newAngle + 360 else if (newAngle > 360) newAngle - 360 else newAngle
    
    
    val newDirection = normalisedAngle match {
      case 0 | 360 => Direction.East
      case 90 => Direction.North
      case 180 => Direction.West
      case 270 => Direction.South
    }

    newDirection
  }

  def answer: Long = {
    val lines = util.readFileLines("data/input/day12.txt")

    val moves: List[MoveDirection] = lines.map(line =>
      line match {
        case s"N${value}" => MoveDirection.North(value.toInt)
        case s"S${value}" => MoveDirection.South(value.toInt)
        case s"W${value}" => MoveDirection.West(value.toInt)
        case s"E${value}" => MoveDirection.East(value.toInt)
        case s"L${value}" => MoveDirection.Left(value.toInt)
        case s"R${value}" => MoveDirection.Right(value.toInt)
        case s"F${value}" => MoveDirection.Forward(value.toInt)
      }
    )
    
    
//     part 1
    val resolve = moves.map(moveDirection => ShipState.move(_, moveDirection)).reduce(_ andThen _)
    val initialState = ShipState(0, 0, Direction.East)
    val result = resolve(initialState)

    println(result)

    val answer1 = Math.abs(result.x) + Math.abs(result.y)
    
    val resolve2 = moves.map(moveDirection =>   State.move(_, moveDirection)).reduce(_ andThen _)
    val initialState2 = State(Ship(0, 0), Waypoint(10, 1))
    val result2 = resolve2(initialState2)
    
    println(result2)
    
    val answer2 = Math.abs(result2.ship.x) + Math.abs(result2.ship.y)
    
    answer2
  }

  println(answer)

}
