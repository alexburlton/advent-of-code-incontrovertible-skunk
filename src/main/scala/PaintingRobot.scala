import java.awt.Point
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class PaintingRobot(initialMemory: List[Long], initialColour: Int) {
  val computer = new IntcodeComputer(initialMemory)

  val pointsVisited: ListBuffer[Point] = ListBuffer[Point]()
  var direction: Point = new Point(0, -1) //Up
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

  private def doTurn(turnDirection: Int) {
    direction = turnDirection match {
      case 0 => new Point(direction.y, -direction.x)
      case 1 => new Point(-direction.y, direction.x)
    }
  }

  private def moveToNewPosition(): Unit = {
    currentPt = new Point(currentPt.x + direction.x, currentPt.y + direction.y)
  }
}
