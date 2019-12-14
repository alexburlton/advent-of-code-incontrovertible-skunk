import org.scalatest.FlatSpec

class Day14Test extends FlatSpec {
  "Example 14.1" should "get the right ore for 1 fuel" in {
    val inputLines = AdventUtils.readFile("testInputs/Day14.1")
    val map = Day14Helpers.constructMap(inputLines)

    assert(Day14Helpers.getOreToProduceFuel(1, map) == 13312)
  }

  "Example 14.2" should "get the right ore for 1 fuel" in {
    val inputLines = AdventUtils.readFile("testInputs/Day14.2")
    val map = Day14Helpers.constructMap(inputLines)

    assert(Day14Helpers.getOreToProduceFuel(1, map) == 180697)
  }

  "Example 14.3" should "get the right ore for 1 fuel" in {
    val inputLines = AdventUtils.readFile("testInputs/Day14.3")
    val map = Day14Helpers.constructMap(inputLines)

    assert(Day14Helpers.getOreToProduceFuel(1, map) == 2210736)
  }

  "Example 14.1" should "get the right fuel from 1 trillion ore" in {
    val inputLines = AdventUtils.readFile("testInputs/Day14.1")
    val map = Day14Helpers.constructMap(inputLines)

    Day14Helpers.getMinimumRequirements(ElementAndQuantity(12920907875L, "DCFZ"), map)
    assert(Day14Helpers.findFuelProducedByOneTrillionOre(map) == 82892753)
  }

  "Example 14.2" should "get the right fuel from 1 trillion ore" in {
    val inputLines = AdventUtils.readFile("testInputs/Day14.2")
    val map = Day14Helpers.constructMap(inputLines)

    assert(Day14Helpers.findFuelProducedByOneTrillionOre(map) == 5586022)
  }

  "Example 14.3" should "get the right fuel from 1 trillion ore" in {
    val inputLines = AdventUtils.readFile("testInputs/Day14.3")
    val map = Day14Helpers.constructMap(inputLines)

    assert(Day14Helpers.findFuelProducedByOneTrillionOre(map) == 460664 )
  }
}
