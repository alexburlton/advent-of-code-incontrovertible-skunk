import java.awt.Point

import scala.collection.immutable.HashSet
import scala.collection.mutable

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

  override def partB(): Any = -1
}
