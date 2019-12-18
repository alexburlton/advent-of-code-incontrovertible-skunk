import java.awt.Point

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Day18 extends AbstractPuzzle(18) {
  val map: mutable.HashMap[Point, Char] = readMap()
  val keyCount: Int = map.values.count(_.isLower)
  var minFoundSoFar = 5000
  val hmSituationToSteps: mutable.HashMap[Situation, Int] = mutable.HashMap[Situation, Int]()
  //3400 is max

  override def partA(): Any = {
    //findASolution()

    val startingPosition = map.find { tuple => tuple._2 == '@' }.get._1

    val potentialMoves = calculateNextMoves(startingPosition, Set[Char]())
    //val doors = findBlockingDoorsInRange(startingPosition, List(), keysFound)
    //val actualMoves = potentialMoves.filter { it => doors.contains(it.key.toUpper) }

    //println(s"$potentialMoves")
    //println(s"Doors are $doors, so only actually going for $acttualMoves")

    val paths = potentialMoves.map { move =>
      iterateMovesUntilAllKeysFound(move.point, move.stepsAway, Set(move.key))
    }

    println(s"$paths")

    paths.min
  }

  private def findASolution(): Unit = {
    val startingPosition = map.find { tuple => tuple._2 == '@' }.get._1

    var potentialMoves = calculateNextMoves(startingPosition, Set[Char]())
    val doors = findBlockingDoorsInRange(startingPosition, List(), Set[Char]())
    var actualMoves = potentialMoves.filter { it => doors.contains(it.key.toUpper) }
    //var actualMoves = potentialMoves

    val keysFound = mutable.Set[Char]()
    var stepsTaken = 0
    while (keysFound.size < keyCount) {
      val move = actualMoves.minBy(_.stepsAway)
      keysFound.add(move.key)
      potentialMoves = calculateNextMoves(move.point, keysFound.toSet)

      val moreDoors = findBlockingDoorsInRange(startingPosition, List(), Set[Char]())
      actualMoves = potentialMoves.filter { it => moreDoors.contains(it.key.toUpper) }

      if (actualMoves.isEmpty) {
        actualMoves = potentialMoves
      }
      actualMoves = potentialMoves

      stepsTaken += move.stepsAway
    }

    println(s"Naive strategy gets $stepsTaken")
    minFoundSoFar = stepsTaken
  }
  private def iterateMovesUntilAllKeysFound(position: Point, stepsTakenToHere: Int, keysFound: Set[Char]): Int = {
    if (keysFound.size == keyCount) {
      //Found them all!
      updateMinSoFar(stepsTakenToHere)
      return stepsTakenToHere
    }

    if (stepsTakenToHere > minFoundSoFar) {
      //Return some big number so we stop checking this fork
      //println(s"Taken $stepsTakenToHere - aborting")
      return stepsTakenToHere  * 10
    }

    val situation = Situation(position, keysFound)
    val knownStepsFromHere = hmSituationToSteps.get(situation)

    knownStepsFromHere.foreach { it =>
      //println(s"Know the steps from here, stopping: ${stepsTakenToHere + it}")
      return stepsTakenToHere + it }

    val potentialMoves = calculateNextMoves(position, keysFound)

    //println(s"$keysFound, considering $potentialMoves")

    val minTotalSteps = potentialMoves.map { move =>
      val allKeysFound = keysFound ++ List(move.key)
      iterateMovesUntilAllKeysFound(move.point, move.stepsAway + stepsTakenToHere, allKeysFound) }.min

    hmSituationToSteps.put(situation, minTotalSteps - stepsTakenToHere)

    updateMinSoFar(minTotalSteps)

    minTotalSteps
  }
  private def updateMinSoFar(totalSteps: Int): Unit = {
   if (totalSteps < minFoundSoFar) {
     println(s"New best: $totalSteps")
     minFoundSoFar = totalSteps
   }
  }
  private def calculateNextMoves(position: Point, keys: Set[Char]): Vector[PotentialTarget] = {
    val potentialKeys = findAllKeysInRange(position, Set(), keys, 0)
    val distinctKeys = potentialKeys.map(_.key).distinct
    distinctKeys.map(findMinimumDistance(_, potentialKeys))
  }
  private def findMinimumDistance(key: Char, potentialRoutes: Vector[PotentialTarget]): PotentialTarget = {
    potentialRoutes.filter(_.key == key).minBy(_.stepsAway)
  }

  override def partB(): Any = -1

  private def findBlockingDoorsInRange(currentPosition: Point, pointsVisited: List[Point], currentKeys: Set[Char]): List[Char] = {
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
  private def findAllKeysInRange(currentPosition: Point, pointsVisited: Set[Point], currentKeys: Set[Char], stepsSoFar: Int): Vector[PotentialTarget] = {
    val ret = ListBuffer[PotentialTarget]()
    val neighbours = getAdjacentSpaces(currentPosition, pointsVisited, currentKeys)

    val keys = neighbours.filter { it => it._2.isLower && !currentKeys.contains(it._2) }
    val nonKeys = neighbours.filterNot { keys.contains(_) }

    keys.foreach { key => ret.addOne(PotentialTarget(key._2, key._1, stepsSoFar + 1))}

    nonKeys.foreach { nonKey =>
      val positionsVisited = pointsVisited ++ List(currentPosition)
      ret.addAll(findAllKeysInRange(nonKey._1, positionsVisited, currentKeys, stepsSoFar + 1))
    }

    ret.toVector
  }
  private def getAdjacentSpaces(currentPosition: Point, pointsVisited: Set[Point], currentKeys: Set[Char]): List[(Point, Char)] = {
    val neighbours = Day15Helpers.getNeighbours(currentPosition)

    for {
      point <- neighbours
      pointType = map(point)
      if pointType != '#'
      if !(pointType.isUpper && !currentKeys.contains(pointType.toLower))
      if !pointsVisited.contains(point)
    } yield (point, pointType)
  }
  private def getAdjacentSpacesOrWalls(currentPosition: Point, pointsVisited: List[Point], currentKeys: Set[Char]): List[(Point, Char)] = {
    val neighbours = Day15Helpers.getNeighbours(currentPosition)

    for {
      point <- neighbours
      pointType = map(point)
      if pointType != '#'
      if !pointsVisited.contains(point)
    } yield (point, pointType)
  }

  case class Situation(point: Point, currentKeys: Set[Char])
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
