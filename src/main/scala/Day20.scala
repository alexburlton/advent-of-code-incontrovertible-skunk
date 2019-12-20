import java.awt.Point

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Day20 extends AbstractPuzzle(20) {
  val map: mutable.HashMap[Point, Char] = readMap()
  val centerPoint: Point = readCenterPoint(map)
  val (startingPoint, endingPoint, portals): (Point, Point, Set[Portal]) = findPortals(map, centerPoint)
  var minFoundSoFar = 5000

  case class Portal(label: String, outerPt: Point, innerPt: Point) {
    def findOtherPoint(point: Point): Point = {
      if (point == outerPt) innerPt else outerPt
    }
  }
  case class PotentialTarget(portal: Portal, pt: Point, stepsAway: Int)

//  override def partA(): Any = {
//    val potentialMoves = calculateNextMoves(startingPoint, None, None)
//
//    val paths = potentialMoves.map { move =>
//      iterateMovesUntilAtEnd(move.pt, move.stepsAway, move.portal, None)
//    }
//
//    println(s"$paths")
//
//    paths.min
//  }
  override def partA(): Any = -1

  override def partB(): Any = {
    println(s"$portals")

    val potentialMoves = calculateNextMoves(startingPoint, None, Some(0))

    val paths = potentialMoves.map { move =>
      iterateMovesUntilAtEnd(move.pt, move.stepsAway, move.portal, Some(0))
    }

    println(s"$paths")

    paths.min
  }

  private def iterateMovesUntilAtEnd(position: Point, stepsTakenToHere: Int, lastPortal: Portal, currentLevel: Option[Int]): Int = {
    if (position == endingPoint && onLevelZero(currentLevel)) {
      //Made it!
      updateMinSoFar(stepsTakenToHere)
      return stepsTakenToHere
    }

    val stepsAfterTakingPortal = stepsTakenToHere + 1
    val positionAfterPortal = lastPortal.findOtherPoint(position)
    if (stepsAfterTakingPortal > minFoundSoFar) {
      //Return some big number so we stop checking this fork
      //println(s"Taken $stepsTakenToHere - aborting")
      return stepsAfterTakingPortal  * 100
    }

    val newLevelAfterPortal = getNewLevelAfterPortal(positionAfterPortal, lastPortal, currentLevel)

    val potentialMoves = calculateNextMoves(positionAfterPortal, Some(lastPortal), newLevelAfterPortal)
    if (potentialMoves.isEmpty) {
      //Hit a dead end by entering the outer level where we can't hit ZZ. Big number time...
      return minFoundSoFar * 100
    }

    val minTotalSteps = potentialMoves.map { move =>
      iterateMovesUntilAtEnd(move.pt, move.stepsAway + stepsAfterTakingPortal, move.portal, newLevelAfterPortal) }.min

    updateMinSoFar(minTotalSteps)

    minTotalSteps
  }
  private def onLevelZero(currentLevel: Option[Int]): Boolean = {
    currentLevel.isEmpty || currentLevel.get == 0
  }
  private def getNewLevelAfterPortal(newPosition: Point, portal: Portal, currentLevel: Option[Int]): Option[Int] = {
    currentLevel match {
      case None => None
      case Some(value) => if (newPosition == portal.outerPt) Some(value + 1) else Some(value - 1)
    }
  }
  private def updateMinSoFar(totalSteps: Int): Unit = {
    if (totalSteps < minFoundSoFar) {
      println(s"New best: $totalSteps")
      minFoundSoFar = totalSteps
    }
  }
  private def calculateNextMoves(position: Point, lastPortal: Option[Portal], currentLevel: Option[Int]): Vector[PotentialTarget] = {
    val potentialKeys = findAllKeysInRange(position, Set(), 0)
    println(s"** Calculation for position $position, depth $currentLevel, just come from $lastPortal **")
    println(s"Potential keys: $potentialKeys")

    val distinctKeys = potentialKeys.map(_.portal).distinct
    var distinctTargets = distinctKeys.map(findMinimumDistance(_, potentialKeys))

    val zzRoute = distinctTargets.find { target => target.portal.label == "ZZ" }
    if (zzRoute.isDefined && onLevelZero(currentLevel)) {
      val distance = zzRoute.get.stepsAway
      distinctTargets = distinctTargets.filter { target => target.portal.label == "ZZ" || target.stepsAway < distance }
    }

    println(s"Filtered worse than ZZ: $distinctTargets")

    if (lastPortal.isDefined) {
      distinctTargets = distinctTargets.filterNot(_.portal == lastPortal.get)
    }

    println(s"Filtered last portal: $distinctTargets")

    //Remove the outer non-ZZ portals if we're on level 0
    if (currentLevel.isDefined && currentLevel.get == 0) {
      distinctTargets = distinctTargets.filterNot{ target => target.portal.label != "ZZ" && target.portal.outerPt == target.pt }
    }

    println(s"Filtered outer portals: $distinctTargets")

    distinctTargets
  }
  private def findMinimumDistance(portal: Portal, potentialRoutes: Vector[PotentialTarget]): PotentialTarget = {
    potentialRoutes.filter(_.portal == portal).minBy(_.stepsAway)
  }

  private def findAllKeysInRange(currentPosition: Point, pointsVisited: Set[Point], stepsSoFar: Int): Vector[PotentialTarget] = {
    val ret = ListBuffer[PotentialTarget]()
    val neighbours = getAdjacentSpaces(currentPosition, pointsVisited)

    neighbours.foreach { pt =>
      if (pt == endingPoint) {
        ret.addOne(PotentialTarget(Portal("ZZ", endingPoint, endingPoint), pt, stepsSoFar + 1))
      } else if (portals.exists { portal => portal.outerPt == pt || portal.innerPt == pt }) {
        val portal = portals.find { portal => portal.outerPt == pt || portal.innerPt == pt }.get
        ret.addOne(PotentialTarget(portal, pt, stepsSoFar + 1))
      } else {
        val positionsVisited = pointsVisited ++ List(currentPosition)
        ret.addAll(findAllKeysInRange(pt, positionsVisited, stepsSoFar + 1))
      }
    }

    ret.toVector
  }
  private def getAdjacentSpaces(currentPosition: Point, pointsVisited: Set[Point]): List[Point] = {
    val neighbours = Day15Helpers.getNeighbours(currentPosition)

    for {
      point <- neighbours
      pointType = map(point)
      if pointType == '.'
      if !pointsVisited.contains(point)
    } yield point
  }

  /**
   * Initial parsing
   */
  private def readMap(): mutable.HashMap[Point, Char] = {
    val hmPointToType = new mutable.HashMap[Point, Char]

    for (y <- inputLines.indices) {
      val line = inputLines(y)
      for (x <- line.indices) {
        val char = line.charAt(x)

        hmPointToType.put(new Point(x, y), char)
      }
    }

    hmPointToType
  }

  private def readCenterPoint(map: mutable.HashMap[Point, Char]): Point = {
    val xVals = map.keys.map { pt => pt.x }
    val yVals = map.keys.map { pt => pt.y }
    val xAvg = xVals.sum / xVals.size
    val yAvg = yVals.sum / yVals.size
    new Point(xAvg.toInt, yAvg.toInt)
  }

  private def findPortals(map: mutable.HashMap[Point, Char], centerPt: Point): (Point, Point, Set[Portal]) = {
    val letterSpaces = map.filter { tuple => tuple._2.isUpper }
    val dotSpaces = map.filter { tuple => tuple._2 == '.' }.keySet.toSet
    val portals = mutable.Set[Portal]()

    var startingPoint: Point = new Point(0, 0)
    var endingPoint: Point = new Point(0, 0)

    val hmPortalToOtherPoint = mutable.HashMap[String, Point]()

    for {
      (point, letter) <- letterSpaces
    } {
      val (label, thisPoint) = parseLabelAndAdjacentPoint(point, letter, letterSpaces, dotSpaces)
      if (label == "AA") {
        startingPoint = thisPoint
      } else if (label == "ZZ") {
        endingPoint = thisPoint
      }

      if (portals.count(_.label == label) > 0) {
        //Skip this - we've already considered it.
      } else if (hmPortalToOtherPoint.contains(label)) {
        val otherPoint = hmPortalToOtherPoint(label)
        if (thisPoint != otherPoint) {
          portals.add(makePortal(label, thisPoint, otherPoint, centerPt))
        }
      } else {
        hmPortalToOtherPoint.put(label, thisPoint)
      }
    }

    (startingPoint, endingPoint, portals.toSet)
  }
  private def makePortal(label: String, ptA: Point, ptB: Point, centerPt: Point): Portal = {
    val distA = AdventUtils.getDistanceBetweenPoints(ptA, centerPt)
    val distB = AdventUtils.getDistanceBetweenPoints(ptB, centerPt)

    if (distA > distB) {
      Portal(label, ptA, ptB)
    } else {
      Portal(label, ptB, ptA)
    }
  }
  private def parseLabelAndAdjacentPoint(letterPoint: Point,
                                         letter: Char,
                                         letterSpaces: mutable.HashMap[Point, Char],
                                         dotSpaces: Set[Point]): (String, Point) = {

    val left = new Point(letterPoint.x - 1, letterPoint.y)
    val right = new Point(letterPoint.x + 1, letterPoint.y)
    val up = new Point(letterPoint.x, letterPoint.y - 1)
    val down = new Point(letterPoint.x, letterPoint.y + 1)

    if (letterSpaces.contains(left)) {
      val label = letterSpaces(left).toString + letter
      (label, findDotSpace(letterPoint, left, dotSpaces))
    } else if (letterSpaces.contains(right)) {
      val label = letter.toString + letterSpaces(right)
      (label, findDotSpace(letterPoint, right, dotSpaces))
    } else if (letterSpaces.contains(up)) {
      val label = letterSpaces(up).toString + letter
      (label, findDotSpace(letterPoint, up, dotSpaces))
    } else {
      val label = letter.toString + letterSpaces(down)
      (label, findDotSpace(letterPoint, down, dotSpaces))
    }
  }
  private def findDotSpace(ptA: Point, ptB: Point, dotSpaces: Set[Point]): Point = {
    val xMin = Math.min(ptA.x, ptB.x)
    val xMax = Math.max(ptA.x, ptB.x)
    val yMin = Math.min(ptA.y, ptB.y)
    val yMax = Math.max(ptA.y, ptB.y)

    if (xMin == xMax) {
      if (dotSpaces.contains(new Point(ptA.x, yMin - 1))) {
        new Point(ptA.x, yMin - 1)
      } else {
        new Point(ptA.x, yMax + 1)
      }
    } else {
      if (dotSpaces.contains(new Point(xMin - 1, ptA.y))) {
        new Point(xMin - 1, ptA.y)
      } else {
        new Point(xMax + 1, ptA.y)
      }
    }
  }
}
