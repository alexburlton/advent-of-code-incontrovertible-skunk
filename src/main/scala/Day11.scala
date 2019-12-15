import java.awt.Point

import scala.collection.mutable.ListBuffer

class Day11 extends AbstractPuzzle(11) {

  val inputCommands: List[Long] = AdventUtils.parseAsIntcodeInput(inputLines)

  override def partA(): Any = {
    val robot = new PaintingRobot(inputCommands, 0)
    robot.run()

    robot.countPointsPainted()
  }

  override def partB(): Any = {
    val robot = new PaintingRobot(inputCommands, 1)
    robot.run()

    val map = robot.getColourMap
    AdventUtils.printCoordinateGrid(map, ".")
  }
}
