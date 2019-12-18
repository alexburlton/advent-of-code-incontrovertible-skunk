import java.awt.Point

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Day18 extends AbstractPuzzle(18) {
  val map: mutable.HashMap[Point, Char] = readMap()
  val keyCount: Int = map.values.count(_.isLower)
  var minFoundSoFar: Int = Integer.MAX_VALUE

  override def partA(): Any = {
    val startingPosition = map.find { tuple => tuple._2 == '@' }.get._1

    val keysFound = List[Char]()
    val potentialMoves = calculateNextMoves(startingPosition, keysFound)
    val doors = findBlockingDoorsInRange(startingPosition, List(), keysFound)

    val actualMoves = potentialMoves.filter { it => doors.contains(it.key.toUpper) }

    println(s"$potentialMoves")
    println(s"Doors are $doors, so only actually going for $actualMoves")

    val paths = actualMoves.map { move =>
      iterateMovesUntilAllKeysFound(move.point, move.stepsAway, List(move.key))
    }

    paths.min
  }
  private def iterateMovesUntilAllKeysFound(position: Point, stepsTaken: Int, keysFound: List[Char]): Int = {
    if (keysFound.size == keyCount) {
      //Found them all!
      minFoundSoFar = Math.min(minFoundSoFar, stepsTaken)
      return stepsTaken
    }

    if (stepsTaken > minFoundSoFar) {
      return Integer.MAX_VALUE
    }

    val potentialMoves = calculateNextMoves(position, keysFound)
    potentialMoves.map { move =>
      val allKeysFound = keysFound ++ List(move.key)
      println(s"Found $allKeysFound, steps taken: ${stepsTaken + move.stepsAway}")
      iterateMovesUntilAllKeysFound(move.point, stepsTaken + move.stepsAway, allKeysFound) }.min
  }
  private def calculateNextMoves(position: Point, keys: List[Char]): List[PotentialTarget] = {
    val potentialKeys = findAllKeysInRange(position, List(), keys, 0)
    //println(s"potentialKeys = $potentialKeys")
    val distinctKeys = potentialKeys.map(_.key).distinct
    distinctKeys.map(findMinimumDistance(_, potentialKeys))
  }
  private def findMinimumDistance(key: Char, potentialRoutes: List[PotentialTarget]): PotentialTarget = {
    potentialRoutes.filter(_.key == key).minBy(_.stepsAway)
  }

  override def partB(): Any = -1

  private def findBlockingDoorsInRange(currentPosition: Point, pointsVisited: List[Point], currentKeys: List[Char]): List[Char] = {
    val ret = ListBuffer[Char]()
    val neighbours = getAdjacentSpacesOrWalls(currentPosition, pointsVisited, currentKeys)

    val walls = neighbours.filter { it => it._2.isUpper && !currentKeys.contains(it._2.toLower) }
    val nonWalls = neighbours.filterNot { walls.contains(_) }
    //println(s"$nonWalls")

    walls.foreach { wall => ret.addOne(wall._2) }

    nonWalls.foreach { nonWall =>
      val positionsVisited = pointsVisited ++ List(currentPosition)
      ret.addAll(findBlockingDoorsInRange(nonWall._1, positionsVisited, currentKeys))
    }

    ret.toList
  }
  private def findAllKeysInRange(currentPosition: Point, pointsVisited: List[Point], currentKeys: List[Char], stepsSoFar: Int): List[PotentialTarget] = {
    val ret = ListBuffer[PotentialTarget]()
    val neighbours = getAdjacentSpaces(currentPosition, pointsVisited, currentKeys)

    val keys = neighbours.filter { it => it._2.isLower && !currentKeys.contains(it._2) }
    //println(s"Adjacent keys: $keys, currentKeys: $currentKeys")
    val nonKeys = neighbours.filterNot { keys.contains(_) }

    //println(s"At $currentPosition, keys: $keys, non-keys: $nonKeys")
    keys.foreach { key => ret.addOne(PotentialTarget(key._2, key._1, stepsSoFar + 1))}


    nonKeys.foreach { nonKey =>
      val positionsVisited = pointsVisited ++ List(currentPosition)
      ret.addAll(findAllKeysInRange(nonKey._1, positionsVisited, currentKeys, stepsSoFar + 1))
    }

    ret.toList
  }
  private def getAdjacentSpaces(currentPosition: Point, pointsVisited: List[Point], currentKeys: List[Char]): List[(Point, Char)] = {
    val neighbours = Day15Helpers.getNeighbours(currentPosition)

    for {
      point <- neighbours
      pointType = map(point)
      if pointType != '#'
      if !(pointType.isUpper && !currentKeys.contains(pointType.toLower))
      if !pointsVisited.contains(point)
    } yield (point, pointType)
  }
  private def getAdjacentSpacesOrWalls(currentPosition: Point, pointsVisited: List[Point], currentKeys: List[Char]): List[(Point, Char)] = {
    val neighbours = Day15Helpers.getNeighbours(currentPosition)

    for {
      point <- neighbours
      pointType = map(point)
      if pointType != '#'
      if !pointsVisited.contains(point)
    } yield (point, pointType)
  }

  case class PotentialTarget(key: Char, point: Point, stepsAway: Int)

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
}
