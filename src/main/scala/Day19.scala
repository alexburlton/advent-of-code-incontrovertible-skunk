import scala.collection.mutable
import java.awt.Point

class Day19 extends AbstractPuzzle(19) {
  val intcode: List[Long] = AdventUtils.parseAsIntcodeInput(inputLines)

  override def partA(): Any = {
    val set = constructTractorBeamMap(50)
    set.size
  }

  private def constructTractorBeamMap(size: Int): Set[Point] = {
    val set = mutable.HashSet[Point]()
    for (x <- 0 until size) {
      for (y <- 0 until size) {
        val computer = new IntcodeComputer(intcode, List(x, y))
        computer.process()

        if (computer.outputs.last == 1){
          set.add(new Point(x, y))
        }
      }
    }

    set.toSet
  }

  /**
   * Print the map out so I can look at it and figure out the rules
   */
  /*private def createTractorBeamMap(size: Int): Map[Point, String] = {
    val set = mutable.HashMap[Point, String]()
    for (x <- 0 until size) {
      for (y <- 0 until size) {
        val computer = new IntcodeComputer(intcode, List(x, y))
        computer.process()

        if (computer.outputs.last == 1){
          set.put(new Point(x, y), "#")
        } else {
          set.put(new Point(x, y), ".")
        }
      }
    }

    set.toMap
  }*/

  private def generateTractorBeamMapMyself(size: Int): Set[Point] = {
    val set = mutable.Set[Point]()

    val sequence = List(3, 2, 2, 2, 2)
    var sequencePointer = 0
    var pointsPerRow = 1
    var xOffset = -2
    var yRepeats = sequence(sequencePointer)

    for (y <- 0 until size) {
      if (yRepeats == 0) {
        sequencePointer += 1
        yRepeats = sequence(sequencePointer % sequence.size)
        pointsPerRow += 1
        xOffset += 1
      } else {
        xOffset += 2
      }

      if (xOffset > size + 2) {
        return set.toSet
      }

      for (x <- 0 until pointsPerRow) {
        set.add(new Point(x + xOffset, y))
      }

      yRepeats -= 1
    }

    set.toSet
  }

  override def partB(): Any = {
    val set = generateTractorBeamMapMyself(2000)

    findFullyContainedSquare(set, 100)
  }
  private def findFullyContainedSquare(set: Set[Point], sideLength: Int): Point = {
    var found = false
    var point = set.find(_.y == 540).get
    while (!found) {
      point = getNextPoint(set, point)
      found = foundSquare(set, point, sideLength)
    }

    point
  }
  private def foundSquare(set: Set[Point], cornerPoint: Point, size: Int): Boolean = {
    val requiredSet = mutable.Set[Point]()
    for (y <- 0 until size) {
      for (x <- 0 until size) {
        requiredSet.add(new Point(cornerPoint.x + x, cornerPoint.y + y))
      }
    }

    requiredSet.subsetOf(set)
  }
  private def getNextPoint(set: Set[Point], point: Point): Point = {
    val moveX = new Point(point.x + 1, point.y)
    if (set.contains(new Point(point.x + 1, point.y))) {
      moveX
    } else {
      val y = point.y + 1
      set.filter(_.y == y).minBy(_.x)
    }
  }
}
