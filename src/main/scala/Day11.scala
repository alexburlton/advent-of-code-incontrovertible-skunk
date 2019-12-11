import java.awt.Point

import scala.collection.mutable.ListBuffer

class Day11 extends AbstractPuzzle(11) {

  val inputCommands: List[Long] = inputLines.head.split(",").map(s => s.toLong).toList

  override def partA(): Any = {
    val robot = new PaintingRobot(inputCommands, 0)
    robot.run()

    robot.countPointsPainted()
  }

  override def partB(): Any = {
    val robot = new PaintingRobot(inputCommands, 1)
    robot.run()

    val pts = robot.hmPointToColour.keys

    val xMin = pts.minBy(_.x).x
    val xMax = pts.maxBy(_.x).x
    val yMin = pts.minBy(_.y).y
    val yMax = pts.maxBy(_.y).y

    val yRange = yMax to yMin by -1


    for (y <- yRange) {
      val row = ListBuffer[String]()
      for (x <- xMin to xMax) {
        val colour = robot.getColour(new Point(x, y))
        val colourStr = if (colour == 0) "." else "#"
        row.addOne(colourStr)
      }

      println(row.mkString(""))
    }
  }
}
