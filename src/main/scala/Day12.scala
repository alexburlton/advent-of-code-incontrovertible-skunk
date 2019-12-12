import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Day12 extends AbstractPuzzle(12) {
  override def partA(): Any = {
    val moons = inputLines.map { line => new Moon(line) }

    for (_ <- 0 until 1000) {
      updateVelocities(moons)

      moons.foreach { moon => moon.applyVelocity() }
    }

    moons.map { _.getTotalEnergy }.sum
  }

  override def partB(): Any = {
    val xRepeat = findStepsToRepeat((pt: Point) => pt.x)
    val yRepeat = findStepsToRepeat((pt: Point) => pt.y)
    val zRepeat = findStepsToRepeat((pt: Point) => pt.z)

    getLcm(xRepeat, yRepeat, zRepeat)
  }

  private def updateVelocities(moons: List[Moon]): Unit = {
    for {
      (x, idxX) <- moons.zipWithIndex
      (y, idxY) <- moons.zipWithIndex
      if idxX < idxY
    } {
      x.updateVelocity(y)
      y.updateVelocity(x)
    }
  }

  private def getLcm(x: Long, y: Long, z: Long): Long = {
    val xFactors = factorize(x)
    val yFactors = factorize(y)
    val zFactors = factorize(z)

    val allPrimeFactors = xFactors.keySet ++ yFactors.keySet ++ zFactors.keySet
    val factorsOfLcm = allPrimeFactors.map[Long] { factor => Math.pow(factor, getMaxCount(factor, xFactors, yFactors, zFactors)).toInt }
    factorsOfLcm.product
  }

  @tailrec
  private def factorize(x: Long, prime: Long = 2, map: mutable.HashMap[Long, Int] = mutable.HashMap()): mutable.HashMap[Long, Int] = {
    if (prime * prime > x) {
      incrementMap(map, x)
      map
    } else {
      if (x % prime == 0) {
        incrementMap(map, prime)
        factorize(x / prime, prime, map)
      } else {
        factorize(x, prime + 1, map)
      }
    }
  }
  private def getMaxCount(factor: Long, value: mutable.HashMap[Long, Int], value1: mutable.HashMap[Long, Int], value2: mutable.HashMap[Long, Int]) = {
    val list = List[Int](value.getOrElse(factor, 0), value1.getOrElse(factor, 0), value2.getOrElse(factor, 0))
    list.max
  }

  private def incrementMap(map: mutable.HashMap[Long, Int], factor: Long): Unit = {
    val old = map.getOrElse(factor, 0)
    map.put(factor, old + 1)
  }

  private def findStepsToRepeat(dimension: Point => Int): Long = {
    val moons = inputLines.map { line => new Moon(line) }

    val previousStates = mutable.Set[List[Int]]()

    var foundRepeat: Boolean = false
    var numSteps: Long = 0
    while (!foundRepeat) {
      updateVelocities(moons)

      val state = ListBuffer[Int]()
      moons.foreach { moon =>
        moon.applyVelocity()

        state.addOne(dimension(moon.currentPosition))
        state.addOne(dimension(moon.velocity))
      }

      foundRepeat = previousStates.contains(state.toList)

      previousStates.add(state.toList)
      numSteps += 1
    }

    numSteps - 1
  }
}

case class Point(x: Int, y: Int, z: Int) {
  def absoluteSize(): Int = Math.abs(x) + Math.abs(y) + Math.abs(z)
  def signDiff(pt: Point): Point = Point(x - pt.x compare 0, y - pt.y compare 0, z - pt.z compare 0)
}

class Moon(initialCoords: String) {
  var currentPosition: Point = parseInitialCoords(initialCoords)
  var velocity: Point = Point(0, 0, 0)

  private def parseInitialCoords(str: String): Point = {
    val regex = """^<x=(.*), y=(.*), z=(.*)>$""".r("x", "y", "z")

    val regex(x, y, z) = str
    Point(x.toInt, y.toInt, z.toInt)
  }

  def updateVelocity(otherMoon: Moon): Unit = {
    val diff = currentPosition.signDiff(otherMoon.currentPosition)
    velocity = Point(velocity.x - diff.x, velocity.y - diff.y, velocity.z - diff.z)
  }

  def applyVelocity() {
    currentPosition = Point(currentPosition.x + velocity.x, currentPosition.y + velocity.y, currentPosition.z + velocity.z)
  }

  def getTotalEnergy: Int = {
    val pe = currentPosition.absoluteSize()
    val ke = velocity.absoluteSize()

    pe * ke
  }
}