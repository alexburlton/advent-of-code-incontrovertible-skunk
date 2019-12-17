import java.awt.Point

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Day17 extends AbstractPuzzle(17) {
  val intcode: List[Long] = AdventUtils.parseAsIntcodeInput(inputLines)
  val map: mutable.HashMap[Point, Char]  = buildStartingMap()

  override def partA(): Any = {
    //AdventUtils.printCoordinateGrid(map.toMap, ".")

    val intersections = for {
      (point, value) <- map
      if value == '#'
      if isIntersection(point, map)
    } yield point.x * point.y

    intersections.sum
  }

  private def isIntersection(point: Point, map: mutable.HashMap[Point, Char]): Boolean = {
    map.getOrElse(new Point(point.x, point.y - 1), '.') == '#' &&
    map.getOrElse(new Point(point.x, point.y + 1), '.') == '#' &&
      map.getOrElse(new Point(point.x + 1, point.y), '.') == '#' &&
      map.getOrElse(new Point(point.x - 1, point.y), '.') == '#'
  }

  private def buildStartingMap(): mutable.HashMap[Point, Char] = {
    val computer = new IntcodeComputer(intcode)
    computer.process()
    val outputs = computer.outputs

    val map = mutable.HashMap[Point, Char]()
    var currentPt = new Point(0, 0)
    outputs.foreach { it =>
      if (it == 10) {
        //new line
        currentPt = new Point(0, currentPt.y + 1)
      } else {
        map.put(currentPt, it.toChar)
        currentPt = new Point(currentPt.x + 1, currentPt.y)
      }
    }

    map
  }

  /**
   * Path is:
   *
   * R, 6, L, 6, L, 10, L, 8, L, 6, L, 10, L, 6, R, 6, L, 6, L, 10, L, 8, L, 6, L, 10, L, 6, R, 6, L, 8, L, 10, R, 6,
   * R, 6, L, 6, L, 10, L, 8, L, 6, L, 10, L, 6, R, 6, L, 8, L, 10, R, 6, R, 6, L, 6, L, 10, R, 6, L, 8, L, 10, R, 6
   *
   * which becomes:
   *
   * A, C, A, C, B, A, C, B, A, B
   *
   * where:
   *
   * A: R, 6, L, 6, L, 10
   * B: R, 6, L, 8, L, 10, R, 6
   * C: L, 8, L, 6, L, 10, L, 6
   */
  override def partB(): Any = {
    generatePath()

    val mainRoutine = "A,C,A,C,B,A,C,B,A,B"
    val functionA = "R,6,L,6,L,10"
    val functionB = "R,6,L,8,L,10,R,6"
    val functionC = "L,8,L,6,L,10,L,6"

    val inputs = convertInstructionStringToCharInput(mainRoutine) ++
      convertInstructionStringToCharInput(functionA) ++
      convertInstructionStringToCharInput(functionB) ++
      convertInstructionStringToCharInput(functionC) ++
      List(110L, 10L) //no thanks to live feed

    val computer = new IntcodeComputer(intcode, inputs)
    computer.substituteValue(0, 2)
    computer.process()

    computer.outputs.last
  }
  private def convertInstructionStringToCharInput(str: String): List[Long] = {
    str.toCharArray.toList.map { _.toLong } ++ List(10)
  }

  /**
   * TODO - unfold
   */
  private def generatePath(): Unit = {
    val robot = map.find { case (pt: Point, value: Char) => value == '^' || value == 'v' || value == '<' || value == '>' }.get

    val instructions = ListBuffer[String]()
    instructions.addOne("R")

    var currentPt = robot._1
    var currentDirection: Char = '>'
    var deadEnd = false

    while (!deadEnd) {
      val count = countPointsForDirection(currentPt, currentDirection)
      instructions.addOne(s"$count")

      currentPt = applyDirection(currentPt, currentDirection, count)
      val possibleNewDirection = doTurn(currentPt, currentDirection)

      possibleNewDirection match {
        case None => deadEnd = true
        case Some(direction) =>
          instructions.addOne(getTurnInstruction(currentDirection, direction))
          currentDirection = direction
      }
    }
  }

  private def countPointsForDirection(pt: Point, direction: Char): Int = {
    var count = 0
    var ptToCheck = applyDirection(pt, direction, count + 1)
    while (map.getOrElse(ptToCheck, '.') == '#') {
      count += 1
      ptToCheck = applyDirection(pt, direction, count + 1)
    }

    count
  }
  private def applyDirection(pt: Point, direction: Char, amount: Int): Point = {
    direction match {
      case '^' => new Point(pt.x, pt.y - amount)
      case 'v' => new Point(pt.x, pt.y + amount)
      case '>' => new Point(pt.x + amount, pt.y)
      case '<' => new Point(pt.x - amount, pt.y)
    }
  }

  private def getOrthogonalDirections(direction: Char): List[Char] = {
    direction match {
      case '^' | 'v' => List('>', '<')
      case '>' | '<' => List('^', 'v')
    }
  }

  private def doTurn(pt: Point, direction: Char): Option[Char] = {
    val possibleMoves = getOrthogonalDirections(direction)
    possibleMoves.find { dir =>
      val newPt = applyDirection(pt, dir, 1)
      val newTileType = getTileType(newPt)
      newTileType == '#'
    }
  }

  private def getTileType(pt: Point): Char = map.getOrElse(pt, '.')

  private def getTurnInstruction(current: Char, newDir: Char): String = {
    current match {
      case '^' => if (newDir == '<') "L" else "R"
      case 'v' => if (newDir == '>') "L" else "R"
      case '>' => if (newDir == '^') "L" else "R"
      case '<' => if (newDir == 'v') "L" else "R"
    }
  }
}
