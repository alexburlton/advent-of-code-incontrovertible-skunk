import java.awt.Point

import scala.collection.mutable.ListBuffer
import scala.io.Source

object AdventUtils {
  def readFile(filename: String): List[String] = {
    val src = Source.fromFile(filename)
    val list = src.getLines.toList
    src.close
    list
  }

  def parseAsIntcodeInput(inputLines: List[String]): List[Long] = inputLines.head.split(",").map(s => s.toLong).toList

  def toCoordinateList[V](map: Map[Point, V], default: V): List[List[V]] = {
    val allPoints = map.keys
    val xRange = allPoints.minBy(_.x).x to allPoints.maxBy(_.x).x
    val yRange = allPoints.minBy(_.y).y to allPoints.maxBy(_.y).y

    val listBuffer = ListBuffer[List[V]]()
    for (y <- yRange) {
      val list = ListBuffer[V]()
      for (x <- xRange) {
        val pt = new Point(x, y)
        val state = map.getOrElse(pt, default)

        list.addOne(state)
      }
      listBuffer.addOne(list.toList)
    }

    listBuffer.toList
  }
  def printCoordinateGrid[V](map: Map[Point, V], default: V): Unit = {
    val coordList = toCoordinateList(map, default)
    coordList.foreach { list => println(list.mkString) }
  }

  def getDistanceBetweenPoints(ptA: Point, ptB: Point): Double = {
    val xLength = Math.abs(ptA.x - ptB.x)
    val yLength = Math.abs(ptA.y - ptB.y)
    Math.sqrt(xLength * xLength + yLength * yLength)
  }
}

