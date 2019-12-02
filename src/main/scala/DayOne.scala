class DayOne extends AbstractPuzzle(1)
{
  override def partA(): Int = {
    var fuel = 0
    inputLines.foreach { it => fuel += calculateFuelForMass(it.toInt) }
    fuel
  }

  override def partB(): Int = {
    var adjustedFuel = 0
    inputLines.foreach { it => adjustedFuel += calculateAdjustedFuelForMass(it.toInt) }
    adjustedFuel
  }

  def addExtraFuel(initialFuel: Int): Int = {
    var totalFuel = initialFuel
    var fuelToAdd = calculateFuelForMass(initialFuel)
    while (fuelToAdd > 0) {
      totalFuel += fuelToAdd
      fuelToAdd = calculateFuelForMass(fuelToAdd)
    }

    totalFuel
  }

  def calculateFuelForMass(mass: Int): Int = {
     mass / 3 - 2
  }

  def calculateAdjustedFuelForMass(mass: Int): Int = {
    val initialFuel = calculateFuelForMass(mass)
    addExtraFuel(initialFuel)
  }
}
