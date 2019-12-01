import org.scalatest.FlatSpec

class DayOneTest extends FlatSpec {
  "Calculating fuel for mass" should "get the right example answers" in {
    assert(DayOne.calculateFuelForMass(12) == 2)
    assert(DayOne.calculateFuelForMass(14) == 2)
    assert(DayOne.calculateFuelForMass(1969) == 654)
    assert(DayOne.calculateFuelForMass(100756) == 33583)
  }

  "Calculating extra fuel" should "get the right example answers" in {
    assert(DayOne.addExtraFuel(2) == 2)
    assert(DayOne.addExtraFuel(654) == 966)
    assert(DayOne.addExtraFuel(33583) == 50346)
  }
}
