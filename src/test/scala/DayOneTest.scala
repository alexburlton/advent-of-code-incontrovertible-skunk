import org.scalatest.FlatSpec

class DayOneTest extends FlatSpec {
  "Calculating fuel for mass" should "get the right example answers" in {
    assert(new DayOne().calculateFuelForMass(12) == 2)
    assert(new DayOne().calculateFuelForMass(14) == 2)
    assert(new DayOne().calculateFuelForMass(1969) == 654)
    assert(new DayOne().calculateFuelForMass(100756) == 33583)
  }

  "Calculating extra fuel" should "get the right example answers" in {
    assert(new DayOne().addExtraFuel(2) == 2)
    assert(new DayOne().addExtraFuel(654) == 966)
    assert(new DayOne().addExtraFuel(33583) == 50346)
  }
}
