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
    
    for (y <- robot.getYRange) {
      val row = ListBuffer[String]()
      for (x <- robot.getXRange) {
        val colour = robot.getColour(new Point(x, y))
        val colourStr = if (colour == 0) "." else "#"
        row.addOne(colourStr)
      }

      println(row.mkString(""))
    }
  }
}
