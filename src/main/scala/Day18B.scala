import java.awt.Point

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Day18B {
  val inputLines = AdventUtils.readFile("inputs/Day18B")

  val map: mutable.HashMap[Point, Char] = readMap()
  val startingPositions = map.filter { tuple => tuple._2 == '@' }.keys.toList
  val keyCount: Int = map.values.count(_.isLower)
  var minFoundSoFar = 5000
  val hmSituationToSteps: mutable.HashMap[Situation, Int] = mutable.HashMap[Situation, Int]()

  def run(): Any = {
    findASolution()

    val potentialMoves = calculateNextMoves(startingPositions, Set[Char]())

    val paths = potentialMoves.map { move =>
      val newPositions = ListBuffer[Point]()
      newPositions.addAll(startingPositions)
      newPositions(move.droneIx) = move.point
      iterateMovesUntilAllKeysFound(newPositions.toList, move.stepsAway, Set(move.key))
    }

    println(s"$paths")
    println(minFoundSoFar)
  }

  private def findASolution(): Unit = {
    val positions = ListBuffer[Point]()
    positions.addAll(startingPositions)

    println(s"Drone positions: $positions")

    var potentialMoves = calculateNextMoves(positions.toList, Set[Char]())

    println(s"Potential moves: $potentialMoves")

    val keysFound = mutable.Set[Char]()
    var stepsTaken = 0
    while (keysFound.size < keyCount) {
      val move = potentialMoves.minBy(_.stepsAway)
      keysFound.add(move.key)

      positions(move.droneIx) = move.point

      potentialMoves = calculateNextMoves(positions.toList, keysFound.toSet)

      println("#######################################")
      println(s"Opted for key ${move.key}")
      println(s"Drone positions: $positions")
      println(s"Potential moves: $potentialMoves")

      stepsTaken += move.stepsAway
    }

    println(s"Naive strategy gets $stepsTaken")
    minFoundSoFar = stepsTaken
  }
  private def iterateMovesUntilAllKeysFound(positions: List[Point], stepsTakenToHere: Int, keysFound: Set[Char]): Int = {
    if (keysFound.size == keyCount) {
      //Found them all!
      updateMinSoFar(stepsTakenToHere)
      return stepsTakenToHere
    }

//    if (stepsTakenToHere > minFoundSoFar) {
//      //Return some big number so we stop checking this fork
//      //println(s"Taken $stepsTakenToHere - aborting")
//      return stepsTakenToHere  * 10
//    }

    val situation = Situation(positions, keysFound)
    val knownStepsFromHere = hmSituationToSteps.get(situation)

    knownStepsFromHere.foreach { it =>
      //println(s"Know the steps from here, stopping: ${stepsTakenToHere + it}")
      return stepsTakenToHere + it }

    val potentialMoves = calculateNextMoves(positions, keysFound)

    //println(s"$keysFound, considering $potentialMoves")

    val minTotalSteps = potentialMoves.map { move =>
      val allKeysFound = keysFound ++ List(move.key)
      val newPositions = ListBuffer[Point]()
      newPositions.addAll(positions)
      newPositions(move.droneIx) = move.point

      iterateMovesUntilAllKeysFound(newPositions.toList, move.stepsAway + stepsTakenToHere, allKeysFound) }.min

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
  private def calculateNextMoves(positions: List[Point], keys: Set[Char]): Vector[PotentialTarget] = {
    positions.zipWithIndex.flatMap { case (pt: Point, ix: Int) =>
      val potentialKeys = findAllKeysInRange(pt, Set(), keys, 0, ix)
      val distinctKeys = potentialKeys.map(_.key).distinct
      distinctKeys.map(findMinimumDistance(_, potentialKeys))
    }.toVector
  }
  private def findMinimumDistance(key: Char, potentialRoutes: Vector[PotentialTarget]): PotentialTarget = {
    potentialRoutes.filter(_.key == key).minBy(_.stepsAway)
  }

  private def findAllKeysInRange(currentPosition: Point, pointsVisited: Set[Point], currentKeys: Set[Char], stepsSoFar: Int, droneIndex: Int): Vector[PotentialTarget] = {
    val ret = ListBuffer[PotentialTarget]()
    val neighbours = getAdjacentSpaces(currentPosition, pointsVisited, currentKeys)

    val keys = neighbours.filter { it => it._2.isLower && !currentKeys.contains(it._2) }
    val nonKeys = neighbours.filterNot { keys.contains(_) }

    keys.foreach { key => ret.addOne(PotentialTarget(droneIndex, key._2, key._1, stepsSoFar + 1))}

    nonKeys.foreach { nonKey =>
      val positionsVisited = pointsVisited ++ List(currentPosition)
      ret.addAll(findAllKeysInRange(nonKey._1, positionsVisited, currentKeys, stepsSoFar + 1, droneIndex))
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

  case class Situation(positions: List[Point], currentKeys: Set[Char])
  case class PotentialTarget(droneIx: Int, key: Char, point: Point, stepsAway: Int)

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
