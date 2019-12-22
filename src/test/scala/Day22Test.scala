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

  "Constructed polynomial for part one example" should "match manually applied instructions" in {
    val instructions = AdventUtils.readFile("inputs/Day22")
    val size = BigInt(10007L)
    val (a, b) = Day22Helpers.constructPolynomialForReverse(instructions, size)

    val index = BigInt(6526)
    val f_1 = Day22Helpers.applyInversedInstructions(instructions, size, index)
    val f_2 = Day22Helpers.applyInversedInstructions(instructions, size, f_1)
    val f_3 = Day22Helpers.applyInversedInstructions(instructions, size, f_2)

    val calculated_f_1 = a.*(index).+(b).mod(size)
    val calculated_f_2 = a.*(calculated_f_1).+(b).mod(size)
    val calculated_f_3 = a.*(calculated_f_2).+(b).mod(size)

    assert(f_1 == calculated_f_1)
    assert(f_2 == calculated_f_2)
    assert(f_3 == calculated_f_3)
  }

  "Applying polynomial n times at once" should "match result from applying iteratively" in {
    val instructions = AdventUtils.readFile("inputs/Day22")
    val size = BigInt(10007L)
    val (a, b) = Day22Helpers.constructPolynomialForReverse(instructions, size)

    val index = BigInt(6526)

    val f_1 = a.*(index).+(b).mod(size)
    val f_2 = a.*(f_1).+(b).mod(size)
    val manual_f_3 = a.*(f_2).+(b).mod(size)

    val calculated_f_3 = Day22Helpers.applyLinearModuloFunction(a, b, 3, size, index)
    assert(calculated_f_3 == manual_f_3)
  }

  "Applying polynomial n times at once" should "match result from applying iteratively - large deck" in {
    val instructions = AdventUtils.readFile("inputs/Day22")
    val size = BigInt(119315717514047L)
    val (a, b) = Day22Helpers.constructPolynomialForReverse(instructions, size)

    val index = BigInt(2020)

    val f_1 = a.*(index).+(b).mod(size)
    val f_2 = a.*(f_1).+(b).mod(size)
    val manual_f_3 = a.*(f_2).+(b).mod(size)

    val calculated_f_3 = Day22Helpers.applyLinearModuloFunction(a, b, 3, size, index)
    assert(calculated_f_3 == manual_f_3)
  }

  "Applying polynomial for part 2" should "get the right answer" in {
    val instructions = AdventUtils.readFile("inputs/Day22")
    val size = BigInt(119315717514047L)
    val (a, b) = Day22Helpers.constructPolynomialForReverse(instructions, size)

    val index = BigInt(2020)

    val numberAtIndex2020 = Day22Helpers.applyLinearModuloFunction(a, b, BigInt(101741582076661L), size, index)
    assert(numberAtIndex2020 == BigInt(79855812422607L))
  }

  "modPow algorithm" should "get answers that match Python implementation" in {
    assert(Day22Helpers.modPow(6844, 2, 10007) == 7576)
    assert(Day22Helpers.modPow(66, BigInt(119315717514045L), BigInt(119315717514047L)) == BigInt(1807813901728L))
  }

  "modular inverse" should "match python output for power" in {
    assert(Day22Helpers.moduloInverse(66, BigInt(119315717514047L)) == BigInt(1807813901728L))
  }
}
