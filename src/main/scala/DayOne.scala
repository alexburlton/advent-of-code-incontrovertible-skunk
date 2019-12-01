object DayOne
{
  def run(): Unit = {
    val input = Main.readFile("DayOne.txt")
    var initialFuel = 0
    input.foreach { it => initialFuel += calculateFuelForMass(it.toInt) }

    println("1A: " + initialFuel)

    var adjustedFuel = 0
    var newSum = 0
    input.foreach { it => newSum += calculateAdjustedFuelForMass(it.toInt) }

    println("1B: " + newSum)
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
