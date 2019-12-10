import java.awt.Point

import org.scalatest.FlatSpec

class DayTenTest extends FlatSpec {
  "Example inputs" should "yield the correct centerPoint and asteroid counts" in {
    val example1 = DayTenHelpers.getBestCenterAsteroid(AdventUtils.readFile("testInputs/Day10.1"))
    assert(example1.centerPt == new Point(3, 4))
    assert(example1.countPointsVisibleFromCenter() == 8)

    val example2 = DayTenHelpers.getBestCenterAsteroid(AdventUtils.readFile("testInputs/Day10.2"))
    assert(example2.centerPt == new Point(5, 8))
    assert(example2.countPointsVisibleFromCenter() == 33)

    val example3 = DayTenHelpers.getBestCenterAsteroid(AdventUtils.readFile("testInputs/Day10.3"))
    assert(example3.centerPt == new Point(1, 2))
    assert(example3.countPointsVisibleFromCenter() == 35)

    val example4 = DayTenHelpers.getBestCenterAsteroid(AdventUtils.readFile("testInputs/Day10.4"))
    assert(example4.centerPt == new Point(6, 3))
    assert(example4.countPointsVisibleFromCenter() == 41)

    val example5 = DayTenHelpers.getBestCenterAsteroid(AdventUtils.readFile("testInputs/Day10.5"))
    assert(example5.centerPt == new Point(11, 13))
    assert(example5.countPointsVisibleFromCenter() == 210)
  }

  "Shot asteroids" should "be calculated in the right order for the large day 10 example" in {
    val example5 = DayTenHelpers.getBestCenterAsteroid(AdventUtils.readFile("testInputs/Day10.5"))
    val shotAsteroids = example5.getAsteroidShootingOrder

    assert(shotAsteroids.head == new Point(11, 12))
    assert(shotAsteroids(1) == new Point(12, 1))
    assert(shotAsteroids(2) == new Point(12, 2))
    assert(shotAsteroids(9) == new Point(12, 8))
    assert(shotAsteroids(19) == new Point(16, 0))
    assert(shotAsteroids(49) == new Point(16, 9))
    assert(shotAsteroids(99) == new Point(10, 16))
    assert(shotAsteroids(198) == new Point(9, 6))
    assert(shotAsteroids(199) == new Point(8, 2))
    assert(shotAsteroids(200) == new Point(10, 9))
    assert(shotAsteroids.last == new Point(11, 1))
  }
}
