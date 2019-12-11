import java.awt.Point

import Direction.Direction

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Direction extends Enumeration {
  type Direction = Value
  val UP, DOWN, LEFT, RIGHT = Value
}

class PaintingRobot(initialMemory: List[Long], initialColour: Int) {
  val computer = new IntcodeComputer(initialMemory)

  val pointsVisited: ListBuffer[Point] = ListBuffer[Point]()
  var direction: Direction = Direction.UP
  var currentPt: Point = new Point(0, 0)
  var outputPointer = 0

  val hmPointToColour: mutable.HashMap[Point, Int] = mutable.HashMap[Point, Int]()

  def run(): Unit = {
    hmPointToColour.put(currentPt, initialColour)

    while (!computer.terminate) {
      val currentColour = hmPointToColour.getOrElse(currentPt, 0)

      computer.processWithInput(currentColour)

      val outputs = computer.outputs
      val newColour = outputs(outputPointer)
      val turnDirection = outputs(outputPointer + 1)

      hmPointToColour.put(currentPt, newColour.toInt)
      doTurn(turnDirection.toInt)

      moveToNewPosition()

      outputPointer += 2
    }
  }

  def countPointsPainted(): Int = hmPointToColour.size

  def getColour(pt: Point): Int = hmPointToColour.getOrElse(pt, 0)

  def getXRange: Range = {
    val pts = hmPointToColour.keys
    val xMin = pts.minBy(_.x).x
    val xMax = pts.maxBy(_.x).x

    xMin to xMax
  }

  def getYRange: Range = {
    val pts = hmPointToColour.keys
    val yMin = pts.minBy(_.y).y
    val yMax = pts.maxBy(_.y).y

    yMin to yMax
  }

  private def doTurn(turnDirection: Int): Unit = {
    if (turnDirection == 0) {
      direction = direction match {
        case Direction.UP => Direction.LEFT
        case Direction.LEFT => Direction.DOWN
        case Direction.DOWN => Direction.RIGHT
        case Direction.RIGHT => Direction.UP
      }
    } else {
      direction = direction match {
        case Direction.UP => Direction.RIGHT
        case Direction.RIGHT => Direction.DOWN
        case Direction.DOWN => Direction.LEFT
        case Direction.LEFT => Direction.UP
      }
    }
  }

  private def moveToNewPosition(): Unit = {
    currentPt = direction match {
      case Direction.UP => new Point(currentPt.x, currentPt.y - 1)
      case Direction.DOWN => new Point(currentPt.x, currentPt.y + 1)
      case Direction.RIGHT => new Point(currentPt.x + 1, currentPt.y)
      case Direction.LEFT => new Point(currentPt.x - 1, currentPt.y)
    }
  }
}
