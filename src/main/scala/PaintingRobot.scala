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

  def getColourMap: Map[Point, String] = {
    hmPointToColour.view.mapValues { col: Int => getColourStr(col) }.toMap
  }
  private def getColourStr(colour: Int): String = if (colour == 0) "." else "'"

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
