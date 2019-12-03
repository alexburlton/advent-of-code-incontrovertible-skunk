import java.awt.Point

import scala.collection.mutable

class DayThree extends AbstractPuzzle(3) {
  override def partA(): Int = {
    val wireA = new Wire(inputLines.head)
    val wireB = new Wire(inputLines(1))

    WireHelpers.getMinCrossingManhattenDistance(wireA, wireB)
  }

  override def partB(): Int = {
    val wireA = new Wire(inputLines.head)
    val wireB = new Wire(inputLines(1))

    WireHelpers.getMinSignalDelay(wireA, wireB)
  }
}

object WireHelpers {

  def getMinCrossingManhattenDistance(wireA: Wire, wireB: Wire): Int = {
    val crossings = WireHelpers.findCrossings(wireA, wireB)
    val distances = crossings.map { WireHelpers.getManhattenDistance }
    distances.min
  }

  def getMinSignalDelay(wireA: Wire, wireB: Wire): Int = {
    val crossings = WireHelpers.findCrossings(wireA, wireB)

    val distances = crossings.map { WireHelpers.getSignalDelay(_, wireA, wireB) }
    distances.min
  }

  def getManhattenDistance(pt: Point): Int = Math.abs(pt.x) + Math.abs(pt.y)

  def getSignalDelay(pt: Point, wireA: Wire, wireB: Wire): Int = wireA.coords.indexOf(pt) + wireB.coords.indexOf(pt) + 2

  def findCrossings(wireA: Wire, wireB: Wire): Vector[Point] = wireA.coords.intersect(wireB.coords)
}

class Wire(path: String) {
  val coords: Vector[Point] = processPath(path.split(",").toList)

  def processPath(path: List[String]): Vector[Point] = {
    val coords = new mutable.ListBuffer[Point]()

    var currentPt = new Point(0, 0)
    path.foreach { move =>
      val direction = move.charAt(0).toString
      val distance = move.substring(1).toInt

      for (_ <- 0 until distance) {
        val newPt = direction match {
          case "U" => new Point(currentPt.x, currentPt.y + 1)
          case "D" => new Point(currentPt.x, currentPt.y - 1)
          case "L" => new Point(currentPt.x - 1, currentPt.y)
          case "R" => new Point(currentPt.x + 1, currentPt.y)
        }

        coords.addOne(newPt)
        currentPt = newPt
      }
    }

    coords.toVector
  }
}
