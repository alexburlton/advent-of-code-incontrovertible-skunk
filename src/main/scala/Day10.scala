import java.awt.Point

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Day10 extends AbstractPuzzle(10) {

  private val TOP_RIGHT = new Quadrant(0, 90, true, yIsPositive = false)
  private val BOTTOM_RIGHT = new Quadrant(90, 180, true, yIsPositive = true)
  private val BOTTOM_LEFT = new Quadrant(180, 270, false, yIsPositive = true)
  private val TOP_LEFT = new Quadrant(270, 360, false, yIsPositive = false)

  private val QUADRANTS = List[Quadrant](TOP_RIGHT, TOP_LEFT, BOTTOM_RIGHT, BOTTOM_LEFT)

  class Quadrant(val minimumAngle : Int, val maximumAngle: Int, val xIsPositive: Boolean, val yIsPositive: Boolean)
  {
    val sinForX: Boolean = xIsPositive ^ yIsPositive
  }


  override def partA(): Any = {
    val pts = getAsteroidPoints()
    pts.map(pt => findAsteroidsICanSee(pt, pts).size).max
  }

  override def partB(): Any = {
    val pts = getAsteroidPoints()
    val maps = pts.map(pt => findAsteroidsICanSee(pt, pts))

    val hmAngleToPoints = maps.maxBy(_.size)
    val allOtherPoints = hmAngleToPoints.values.flatten[Point].toSet
    val centerPtList = pts.filterNot(allOtherPoints)
    println(centerPtList)

    val centerPt = pts.filterNot(allOtherPoints).head
    val sortedAngles: List[Double] = hmAngleToPoints.keys.toList.sortBy { angle => angle }
    println(sortedAngles)

    var steps = 0
    var asteroidVaporized: Point = null
    while (hmAngleToPoints.values.flatten[Point].toSet.nonEmpty) {
      sortedAngles.foreach { angle =>
        val pts = hmAngleToPoints(angle)
        if (pts.nonEmpty) {
          val sortedPoints = pts.sortBy(getDistanceBetweenPoints(centerPt, _))
          asteroidVaporized = sortedPoints.head

          val newPoints = sortedPoints.to(ListBuffer)
          newPoints.remove(0)
          hmAngleToPoints(angle) = newPoints.toList


          println(s"$steps: $angle $sortedPoints -> $newPoints [$asteroidVaporized]")

          steps += 1
        }
      }
    }

    asteroidVaporized
  }

  private def findAsteroidsICanSee(centerPt: Point, allPoints: List[Point]): mutable.HashMap[Double, List[Point]] = {
    val allOtherPoints = allPoints.filterNot(_ == centerPt)
    allOtherPoints.groupBy(pt => getAngleForPoint(pt, centerPt)).to(mutable.HashMap)
  }

  private def getAngleForPoint(dartPt: Point, centerPt: Point): Double = {
      val xLength = dartPt.x - centerPt.x
      val yLength = dartPt.y - centerPt.y
      val hypotenuse = Math.sqrt(xLength * xLength + yLength * yLength)

      if (xLength == 0.0)
      {
        (if (yLength > 0) 180.0 else 0.0)
      }
      else if (yLength == 0.0)
      {
        (if (xLength > 0) 90.0 else 270.0)
      }
      else
      {
        //We're not on an axis relative to the center point
        val quadrant = QUADRANTS.find { q => q.xIsPositive == xLength > 0 && q.yIsPositive == yLength > 0 }.get
        val angleToAdd = quadrant.minimumAngle

        val lengthForCalculation = if (quadrant.sinForX) Math.abs(yLength) else Math.abs(xLength)

        var arcCosValue = Math.acos(lengthForCalculation / hypotenuse)
        arcCosValue = Math.abs(Math.toDegrees(arcCosValue))

        val angle = BigDecimal(angleToAdd + arcCosValue)

        angle.setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
      }
    }

  private def getDistanceBetweenPoints(ptA: Point, ptB: Point): Double = {
      val xLength = Math.abs(ptA.x - ptB.x)
      val yLength = Math.abs(ptA.y - ptB.y)
      Math.sqrt(xLength * xLength + yLength * yLength)
  }

  private def getAsteroidPoints(): List[Point] = {
    val pts = ListBuffer[Point]()
    for (y <- inputLines.indices) {
      val row = inputLines(y).toList
      for (x <- row.indices) {
        val pt = row(x)
        if (pt == '#') {
          pts.addOne(new Point(x, y))
        }
      }
    }

    pts.toList
  }
}
