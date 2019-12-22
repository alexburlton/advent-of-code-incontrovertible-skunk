import org.scalatest.FlatSpec

class Day22Test extends FlatSpec {
  "Applying example instructions in order" should "report the right position for 7" in {
    assert(Day22Helpers.applyInstructions(List(
      "deal with increment 7",
      "deal into new stack",
      "deal into new stack"), 10, 7) == 9)

    assert(Day22Helpers.applyInstructions(List(
      "cut 6",
      "deal with increment 7",
      "deal into new stack"), 10, 7) == 2)

    assert(Day22Helpers.applyInstructions(List(
      "deal with increment 7",
      "deal with increment 9",
      "cut -2"), 10, 7) == 3)

    assert(Day22Helpers.applyInstructions(List(
      "deal into new stack",
      "cut -2",
      "deal with increment 7",
      "cut 8",
      "cut -4",
      "deal with increment 7",
      "cut 3",
      "deal with increment 9",
      "deal with increment 3",
      "cut -1"), 10, 7) == 6)
  }

  "Applying the input for Part A" should "produce the right final index for 2019" in {
    val inputLines = AdventUtils.readFile("inputs/Day22")
    val size = BigInt(10007)
    val index = BigInt(2019)

    assert(Day22Helpers.applyInstructions(inputLines, size, index) == 6526)
  }

  "Applying the input in reverse for Part A" should "get back to 2019 from the answer" in {
    val inputLines = AdventUtils.readFile("inputs/Day22")
    val size = BigInt(10007)
    val index = BigInt(6526)

    assert(Day22Helpers.applyInversedInstructions(inputLines, size, index) == 2019)
  }

  "Applying instructions forwards and backwards" should "get back to the same start point for large numbers" in {
    val inputLines = AdventUtils.readFile("inputs/Day22")
    val size = BigInt(119315717514047L)
    val index = BigInt(2020L)

    val a = Day22Helpers.applyInstructions(inputLines, size, index)
    val b = Day22Helpers.applyInstructions(inputLines, size, a)

    val bInverse = Day22Helpers.applyInversedInstructions(inputLines, size, b)
    val aInverse = Day22Helpers.applyInversedInstructions(inputLines, size, bInverse)

    println(s"$index -> $a -> $b -> $bInverse -> $aInverse")

    assert(index == aInverse)
    assert(a == bInverse)
  }

  /*
  val result = applyInstructions(inputLines, size, index)
    val result2 = applyInstructions(inputLines, size, result)

    val inverted1 = applyInversedInstructions(inputLines, size, result2)
    val inverted = applyInversedInstructions(inputLines, size, inverted1)

    println(s"$index -> $result -> $result2 -> $inverted1 -> $inverted")
   */
}
