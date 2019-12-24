import java.awt.Point

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Day24 extends AbstractPuzzle(24) {

  val startingMap: Map[Point, String] = AdventUtils.parseAsMapFile(inputLines)

  override def partA(): Any = {
    val configsSeen = mutable.HashSet[String]()

    var configStr = convertToString(startingMap)
    var currentMap = startingMap
    while (!configsSeen.contains(configStr)) {
      configsSeen.add(configStr)

      currentMap = iterateGrid(currentMap)
      configStr = convertToString(currentMap)
    }

    AdventUtils.printCoordinateGrid(currentMap, ".")
    getBiodiversityScore(currentMap)
  }

  private def getBiodiversityScore(map: Map[Point, String]): Int = {
    val coords = AdventUtils.toCoordinateList(map, ".").flatten

    coords.zipWithIndex.map { case (str: String, index: Int) =>
      if (str == ".") {
        0
      } else {
        Math.pow(2, index).toInt
      }}.sum
  }

  private def convertToString(map: Map[Point, String]): String = {
    val coords = AdventUtils.toCoordinateList(map, ".")
    coords.flatten.mkString
  }

  private def iterateGrid(map: Map[Point, String]): Map[Point, String] = {
    val newMap = mutable.HashMap[Point, String]()

    map.keys.foreach { point =>
      val bugCount = countBugNeighbours(point, map)
      val myState = map.getOrElse(point, ".")

      if (myState == "#" && bugCount != 1) {
        newMap.put(point, ".")
      } else if (myState == "." && (bugCount == 1 || bugCount == 2)) {
        newMap.put(point, "#")
      } else {
        newMap.put(point, myState)
      }
    }

    newMap.toMap
  }

  private def countBugNeighbours(point: Point, map: Map[Point, String]): Int = {
    val neighbours = Day15Helpers.getNeighbours(point)

    neighbours.map { pt => map.getOrElse(pt, ".") }.count(_ == "#")
  }

  override def partB(): Any = {
    val levelMap = mutable.HashMap[Int, Map[Point, String]]()
    levelMap.put(0, startingMap)

    var newMap = levelMap.toMap

    for (_ <- 0 to 199) {
      newMap = iterateGridRecursive(newMap)
    }

//    newMap.keys.foreach { level =>
//      println(s"LEVEL $level")
//      AdventUtils.printCoordinateGrid(newMap(level), ".")
//    }

    val strs = newMap.values.flatMap(_.values)
    strs.count(_ == "#")
  }

  def iterateGridRecursive(levelMap: Map[Int, Map[Point, String]]): Map[Int, Map[Point, String]] = {

    val extended = extendMap(levelMap)

    val newLevelMap = mutable.HashMap[Int, Map[Point, String]]()

    extended.foreach { case (level: Int, map: Map[Point, String]) =>
      val newMap = mutable.HashMap[Point, String]()
      map.keys.foreach { point =>
        val bugCount = countBugNeighboursRecursive(point, level, extended)
        val myState = map.getOrElse(point, ".")

        if (myState == "#" && bugCount != 1) {
          newMap.put(point, ".")
        } else if (myState == "." && (bugCount == 1 || bugCount == 2)) {
          newMap.put(point, "#")
        } else {
          newMap.put(point, myState)
        }
      }

      newLevelMap.put(level, newMap.toMap)
    }

    newLevelMap.toMap
  }

  private def countBugNeighboursRecursive(point: Point, level: Int, levels: Map[Int, Map[Point, String]]): Int = {
    val neighbours = Day15Helpers.getNeighbours(point)

    val recursedNeighbours = convertNeighboursAcrossLevels(point, neighbours, level)
    recursedNeighbours.map { it => mapPointToStringRecursive(it, levels) }.count(_ == "#")
  }
  private def mapPointToStringRecursive(pointAndLevel: PointAndLevel, levels: Map[Int, Map[Point, String]]): String = {
    if (!levels.contains(pointAndLevel.level)) {
      return "."
    }

    val mapForLevel = levels(pointAndLevel.level)
    mapForLevel.getOrElse(pointAndLevel.pt, ".")
  }

  case class PointAndLevel(pt: Point, level: Int)
  private def convertNeighboursAcrossLevels(point: Point, simpleNeighbours: List[Point], myLevel: Int): List[PointAndLevel] = {
    val list = ListBuffer[PointAndLevel]()

    simpleNeighbours.foreach { pt =>
      if (pt == new Point(2, 2)) {
        //do something bananas, this is the center!
        addPointsFromLevelAbove(point, myLevel, list)
      } else if (pt.x > 4) {
        //We've gone over to the right
        list.addOne(PointAndLevel(new Point(3, 2), myLevel - 1))
      } else if (pt.x < 0) {
        //We've gone over to the left
        list.addOne(PointAndLevel(new Point(1, 2), myLevel - 1))
      } else if (pt.y > 4) {
        //Gone off the bottom
        list.addOne(PointAndLevel(new Point(2, 3), myLevel - 1))
      } else if (pt.y < 0) {
        //Gone off the top
        list.addOne(PointAndLevel(new Point(2, 1), myLevel - 1))
      } else {
        list.addOne(PointAndLevel(pt, myLevel))
      }
    }

    list.toList
  }
  private def addPointsFromLevelAbove(pt: Point, myLevel: Int, list: ListBuffer[PointAndLevel]): Unit = {
    if (pt.x == 3) {
      for (y <- 0 to 4) list.addOne(PointAndLevel(new Point(4, y), myLevel + 1))
    } else if (pt.x == 1) {
      for (y <- 0 to 4) list.addOne(PointAndLevel(new Point(0, y), myLevel + 1))
    } else if (pt.y == 3) {
      for (x <- 0 to 4) list.addOne(PointAndLevel(new Point(x, 4), myLevel + 1))
    } else if (pt.y == 1) {
      for (x <- 0 to 4) list.addOne(PointAndLevel(new Point(x, 0), myLevel + 1))
    }
  }

  private def extendMap(levelMap: Map[Int, Map[Point, String]]): Map[Int, Map[Point, String]] = {
    val minLevel = levelMap.keys.min
    val maxLevel = levelMap.keys.max

    val newLevelMap = mutable.HashMap[Int, Map[Point, String]]()
    newLevelMap.addAll(levelMap)
    newLevelMap.put(minLevel - 1, createEmptyLevel())
    newLevelMap.put(maxLevel + 1, createEmptyLevel())

    newLevelMap.toMap
  }
  private def createEmptyLevel(): Map[Point, String] = {
    val seq = for {
      x <- 0 to 4
      y <- 0 to 4
    } yield new Point(x, y) -> "."

    val map = new mutable.HashMap[Point, String]()
    map.addAll(seq)
    map.put(new Point(2, 2), "?")
    map.toMap
  }
}
