import java.awt.Point

import scala.collection.mutable
import scala.util.Random

case class PotentialMove(direction: Int, knownResult: String)

class Day15 extends AbstractPuzzle(15) {

  val intcode: List[Long] = AdventUtils.parseAsIntcodeInput(inputLines)
  val map = new mutable.HashMap[Point, String]

  override def partA(): Any = {
    Day15Helpers.fullyExploreArea(intcode, map)
    Day15Helpers.findShortestPathToOxygen(map)
  }

  override def partB(): Any = {
    Day15Helpers.fillWithOxygen(map)
  }
}

object Day15Helpers {
  val validDirections: List[Int] = List(1, 2, 3, 4)

  /**
   * Step 1 - Fully explore the map by repeating random walks until we're not discovering anything new
   * Build up a map of point -> tile, where:
   *  O = start
   *  X = oxygen
   *  # = wall
   *  . = empty space
   */
  def fullyExploreArea(intcode: List[Long], hmPointToType: mutable.HashMap[Point, String]): Unit = {
    var previousSize = 0
    hmPointToType.put(new Point(0, 0), "O")

    while (previousSize < hmPointToType.size) {
      previousSize = hmPointToType.size

      doRandomWalk(new IntcodeComputer(intcode), hmPointToType)
    }
  }
  private def doRandomWalk(computer: IntcodeComputer, hmPointToType: mutable.HashMap[Point, String]): Unit = {
    var output: Long = 0
    var previousPosition = new Point(0, 0)
    var currentPosition = new Point(0, 0)

    while (output != 2) {
      val directionToMove = chooseDirectionToMove(currentPosition, previousPosition, hmPointToType)

      val nextPoint = getNextPosition(currentPosition, directionToMove)

      computer.processWithInput(directionToMove)
      output = computer.outputs.last

      if (output == 0) {
        hmPointToType.put(nextPoint, "#")
      } else {
        previousPosition = currentPosition
        currentPosition = nextPoint
        if (!hmPointToType.contains(nextPoint)) {
          val str = if (output == 1) "." else "X"
          hmPointToType.put(nextPoint, str)
        }
      }
    }
  }

  /**
   * Consider the four possible moves from the current position, then:
   *
   *  - Filter out known walls (#)
   *  - Look for unknown results, and if there is one pick it
   *  - If no unknown moves, pick a random direction other than where you were previously
   *  - If only possibility is to go back on yourself, do that.
   */
  private def chooseDirectionToMove(currentPosition: Point, previousPosition: Point, map: mutable.HashMap[Point, String]): Int =
  {
    val validMoves = getPotentialMovesForRandomWalk(currentPosition, map)
    val unknownResults = validMoves.filter(_.knownResult == " ")

    if (unknownResults.nonEmpty) {
      //Go in a direction where the outcome is not yet known
      unknownResults(Random.nextInt(unknownResults.size)).direction
    }
    else
    {
      if (validMoves.size > 1)
      {
        //Multiple valid moves, so filter out the one that would take us back to our previous position and pick one
        val notBackwards = validMoves.filterNot { it =>getNextPosition(currentPosition, it.direction) == previousPosition }
        notBackwards(Random.nextInt(notBackwards.size)).direction
      } else {
        //Only one non-wall, so just go that way
        validMoves.head.direction
      }
    }
  }

  /**
   * Apply the four directions to find the four next points, then filter out any that are walls
   */
  private def getPotentialMovesForRandomWalk(currentPosition: Point, map: mutable.HashMap[Point, String]): List[PotentialMove] = {
    val potentialMoves = validDirections.map { it => getPotentialMove(currentPosition, it, map) }
    potentialMoves.filterNot(_.knownResult == "#")
  }
  private def getPotentialMove(currentPosition: Point, direction: Int, map: mutable.HashMap[Point, String]): PotentialMove = {
    val knownResult = map.getOrElse(getNextPosition(currentPosition, direction), " ")
    PotentialMove(direction, knownResult)
  }

  /**
   * Step 2 - Using the completed map, recursively find all possible paths to X and return the minimum length
   */
  def findShortestPathToOxygen(completeMap: mutable.HashMap[Point, String]): Int = {
    val previousPosition = new Point(0, 0)
    val currentPosition = new Point(0, 0)

    val possibleDirections = getPossibleDirections(currentPosition, previousPosition, completeMap)
    possibleDirections.map { d => getDistanceFromPointToOxygen(0, currentPosition, d.direction, completeMap)}.min
  }
  private def getDistanceFromPointToOxygen(stepsSoFar: Int,
                                           currentPosition: Point,
                                           directionToMove: Int,
                                           completeMap: mutable.HashMap[Point, String]): Int = {

    val newPosition = getNextPosition(currentPosition, directionToMove)

    if (completeMap(newPosition) == "X") {
      return stepsSoFar + 1
    }

    //Not there yet, so keep going
    val possibleDirections = getPossibleDirections(newPosition, currentPosition, completeMap)

    if (possibleDirections.isEmpty) {
      //Dead end
      Integer.MAX_VALUE
    } else {
      possibleDirections.map { d => getDistanceFromPointToOxygen(stepsSoFar + 1, newPosition, d.direction, completeMap)}.min
    }
  }

  /**
   * Gets the non-walls as before, but this time always strip out our previous position.
   * This method will return empty if we have entered a dead-end.
   */
  private def getPossibleDirections(currentPosition: Point, previousPosition: Point, map: mutable.HashMap[Point, String]): List[PotentialMove] =
  {
    getPotentialMovesForRandomWalk(currentPosition, map).filterNot { it => getNextPosition(currentPosition, it.direction) == previousPosition }
  }

  private def getNextPosition(currentPosition: Point, direction: Int): Point = {
    direction match {
      case 1 => new Point(currentPosition.x, currentPosition.y - 1)
      case 2 => new Point(currentPosition.x, currentPosition.y + 1)
      case 3 => new Point(currentPosition.x - 1, currentPosition.y)
      case 4 => new Point(currentPosition.x + 1, currentPosition.y)
    }
  }

  /**
   * Part B - find how many minutes it takes to fill with oxygen.
   * While there are still '.' values in the map, iteratively spread oxygen X's
   */
  def fillWithOxygen(map: mutable.HashMap[Point, String]): Int = {
    //Overwrite the drone with a regular space for this part
    map.put(new Point(0, 0), ".")

    var ticks = 0
    while (map.values.count(_ == ".") > 0) {
      val newMap = mutable.HashMap[Point, String]()
      newMap.addAll(map)
      val oxygenPoints = newMap.filterInPlace { (_: Point, str: String) => str == "X" }.keys.toList

      val nextPoints = oxygenPoints.flatMap { Day15Helpers.getNeighbours }
      nextPoints.foreach { pt =>
        val current = map.getOrElse(pt, " ")
        if (current == ".") {
          map.put(pt, "X")
        }
      }

      ticks += 1
    }

    ticks
  }

  def getNeighbours(point: Point): List[Point] = {
    List(getNextPosition(point, 1), getNextPosition(point, 2), getNextPosition(point, 3), getNextPosition(point, 4))
  }
}
