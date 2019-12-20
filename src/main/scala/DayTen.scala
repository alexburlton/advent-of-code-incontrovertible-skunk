import java.awt.Point

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class AsteroidData(val centerPt: Point, val hmAngleToPoints: Map[Double, List[Point]]) {
  def countPointsVisibleFromCenter(): Int = hmAngleToPoints.size

  def getAllAnglesSorted: List[Double] = hmAngleToPoints.keys.toList.sortBy { angle => angle }

  def getAsteroidShootingOrder: List[Point] = {
    val shotAsteroids = ListBuffer[Point]()

    val sortedAngles = getAllAnglesSorted
    val mutableMap = hmAngleToPoints.to(mutable.HashMap)

    while (mutableMap.values.flatten[Point].nonEmpty) {
      sortedAngles.foreach { angle =>
        val pts = mutableMap(angle)
        if (pts.nonEmpty) {
          val sortedPoints = pts.sortBy(AdventUtils.getDistanceBetweenPoints(centerPt, _))
          val newPoints = sortedPoints.to(ListBuffer)
          val asteroidVaporized = newPoints.remove(0)

          shotAsteroids.addOne(asteroidVaporized)

          mutableMap(angle) = newPoints.toList
        }
      }
    }

    shotAsteroids.toList
  }
}

class DayTen extends AbstractPuzzle(10) {
  override def partA(): Any = {
    val bestAsteroid = DayTenHelpers.getBestCenterAsteroid(inputLines)
    bestAsteroid.countPointsVisibleFromCenter()
  }

  override def partB(): Any = {
    val bestAsteroid = DayTenHelpers.getBestCenterAsteroid(inputLines)

    val shotAsteroids = bestAsteroid.getAsteroidShootingOrder
    val pt = shotAsteroids(199)
    100 * pt.x + pt.y
  }
}

object DayTenHelpers {
  private val TOP_RIGHT = new Quadrant(0, 90, true, yIsPositive = false)
  private val BOTTOM_RIGHT = new Quadrant(90, 180, true, yIsPositive = true)
  private val BOTTOM_LEFT = new Quadrant(180, 270, false, yIsPositive = true)
  private val TOP_LEFT = new Quadrant(270, 360, false, yIsPositive = false)

  private val QUADRANTS = List[Quadrant](TOP_RIGHT, TOP_LEFT, BOTTOM_RIGHT, BOTTOM_LEFT)

  class Quadrant(val minimumAngle: Int, val maximumAngle: Int, val xIsPositive: Boolean, val yIsPositive: Boolean) {
    val sinForX: Boolean = xIsPositive ^ yIsPositive
  }

  def getBestCenterAsteroid(inputLines: List[String]): AsteroidData = {
    val pts = getAsteroidPoints(inputLines)
    val asteroidDatas = pts.map(pt => constructAsteroidData(pt, pts))

    asteroidDatas.maxBy(_.countPointsVisibleFromCenter())
  }

  private def constructAsteroidData(centerPt: Point, allPoints: List[Point]): AsteroidData = {
    val allOtherPoints = allPoints.filterNot(_ == centerPt)
    val hmAngleToPoints = allOtherPoints.groupBy(pt => DayTenHelpers.getAngleForPoint(pt, centerPt))
    new AsteroidData(centerPt, hmAngleToPoints)
  }

  def getAngleForPoint(dartPt: Point, centerPt: Point): Double = {
      val xLength = dartPt.x - centerPt.x
      val yLength = dartPt.y - centerPt.y
      val hypotenuse = Math.sqrt(xLength * xLength + yLength * yLength)

      if (xLength == 0.0)
      {
        if (yLength > 0) 180.0 else 0.0
      }
      else if (yLength == 0.0)
      {
        if (xLength > 0) 90.0 else 270.0
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

  def getAsteroidPoints(inputLines: List[String]): List[Point] = {
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
