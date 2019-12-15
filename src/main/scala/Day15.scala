import java.awt.Point

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

case class PotentialMove(direction: Int, knownResult: String)

class Day15 extends AbstractPuzzle(15) {

  val intcode: List[Long] = AdventUtils.parseAsIntcodeInput(inputLines)

  val validDirections: List[Int] = List(1, 2, 3, 4)

  override def partA(): Any = {
//    val hmPointToType = new mutable.HashMap[Point, String]
//    for (_ <- 0 to 5) {
//      doRandomWalk(hmPointToType)
//    }
//
//    printGrid(hmPointToType)
    -1
  }

  private def doRandomWalk(hmPointToType: mutable.HashMap[Point, String]): mutable.HashMap[Point, String] = {
    val computer = new IntcodeComputer(intcode)

    var output: Long = 0
    var currentPosition = new Point(0, 0)

    hmPointToType.put(currentPosition, "O")
    while (output != 2) {
      val validMoves = getPotentialMoves(currentPosition, hmPointToType)
      val unknownResults = validMoves.filter(_.knownResult == " ")

      val directionToMove = if (unknownResults.nonEmpty) {
        unknownResults(Random.nextInt(unknownResults.size)).direction
      } else {
        validMoves(Random.nextInt(validMoves.size)).direction
      }

      val nextPoint = getNextPosition(currentPosition, directionToMove)

      computer.processWithInput(directionToMove)
      output = computer.outputs.last

      if (output == 0) {
        hmPointToType.put(nextPoint, "#")
      } else {
        currentPosition = nextPoint
        if (!hmPointToType.contains(nextPoint)) {
          val str = if (output == 1) "." else "X"
          hmPointToType.put(nextPoint, str)
        }
      }
    }

    hmPointToType
  }

  private def getPotentialMoves(currentPosition: Point, map: mutable.HashMap[Point, String]): List[PotentialMove] = {
    val potentialMoves = validDirections.map { it => getPotentialMove(currentPosition, it, map) }
    potentialMoves.filterNot(_.knownResult == "#")
  }
  private def getPotentialMove(currentPosition: Point, direction: Int, map: mutable.HashMap[Point, String]): PotentialMove = {
    val knownResult = map.getOrElse(getNextPosition(currentPosition, direction), " ")
    PotentialMove(direction, knownResult)
  }

  private def getNextPosition(currentPosition: Point, direction: Int): Point = {
    direction match {
      case 1 => new Point(currentPosition.x, currentPosition.y - 1)
      case 2 => new Point(currentPosition.x, currentPosition.y + 1)
      case 3 => new Point(currentPosition.x - 1, currentPosition.y)
      case 4 => new Point(currentPosition.x + 1, currentPosition.y)
    }
  }

  private def printGrid(map: mutable.HashMap[Point, String]): Unit = {
    val allPoints = map.keys
    val xRange = allPoints.minBy(_.x).x to allPoints.maxBy(_.x).x
    val yRange = allPoints.minBy(_.y).y to allPoints.maxBy(_.y).y

    for (y <- yRange) {
      val list = ListBuffer[String]()
      for (x <- xRange) {
        val pt = new Point(x, y)
        val state = map.getOrElse(pt, " ")

        list.addOne(state)
      }

      val str = list.mkString
      println(str)
    }
  }

  override def partB(): Any = {
    val mapLines = AdventUtils.readFile("inputs/Day15Map")
    val hmPointToType = new mutable.HashMap[Point, String]

    for (y <- mapLines.indices) {
      val line = mapLines(y)
      for (x <- line.indices) {
        val char = line.charAt(x)

        hmPointToType.put(new Point(x, y), char.toString)
      }
    }

    var ticks = 0
    while (hmPointToType.values.count(_ == ".") > 0) {
      val newMap = mutable.HashMap[Point, String]()
      newMap.addAll(hmPointToType)
      val oxygenPoints = newMap.filterInPlace { (_: Point, str: String) => str == "X" }.keys.toList

      val nextPoints = oxygenPoints.flatMap { getNeighbours }
      nextPoints.foreach { pt =>
        val current = hmPointToType.getOrElse(pt, " ")
        if (current == ".") {
          hmPointToType.put(pt, "X")
        }
      }

      ticks += 1
    }

    printGrid(hmPointToType)
    ticks
    //WTF WHY SCALA
    //hmPointToType.filter { (_: Point, str: String) => str == "X" }
  }
  private def getNeighbours(point: Point): List[Point] = {
    List(getNextPosition(point, 1), getNextPosition(point, 2), getNextPosition(point, 3), getNextPosition(point, 4))
  }
}
