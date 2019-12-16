import org.scalatest.FlatSpec

class Day16Test extends FlatSpec {
  "16 part a examples" should "get the right answers" in {
    assert(Day16Helpers.runPhases("80871224585914546619083218645595", 100) == "24176176")
    assert(Day16Helpers.runPhases("19617804207202209144916044189917", 100) == "73745418")
    assert(Day16Helpers.runPhases("69317163492948606335995924319873", 100) == "52432133")
  }

  "16 part b examples" should "get the right answers" in {
    assert(Day16Helpers.runPhasesWithOffset("03036732577212944063491565474664", 100) == "84462026")
    assert(Day16Helpers.runPhasesWithOffset("02935109699940807407585447034323", 100) == "78725270")
    assert(Day16Helpers.runPhasesWithOffset("03081770884921959731165446850517", 100) == "53553731")
  }
}
