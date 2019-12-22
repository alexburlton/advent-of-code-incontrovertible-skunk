import scala.util.matching.Regex

class Day22 extends AbstractPuzzle(22)
{
  override def partA(): Any = {
    val size = BigInt(10007)
    val index = BigInt(2019)

    Day22Helpers.applyInstructions(inputLines, size, index)
  }

  override def partB(): Any = {
    val size = BigInt(119315717514047L)
    val index = BigInt(2020L)

    val f_1 = Day22Helpers.applyInversedInstructions(inputLines, size, index)
    val f_2 = Day22Helpers.applyInversedInstructions(inputLines, size, f_1)
    val f_3 = Day22Helpers.applyInversedInstructions(inputLines, size, f_2)

    val a_numerator = f_1.-(f_2).%(size)
    val a_denominator = index.-(f_1).%(size)

    println(s"Want multiplicative inverse of $a_denominator mod $size")
    val inverse = Day22Helpers.moduloInverse(a_denominator, size)

    //So this should work...
    val a = a_numerator.*(inverse).%(size)
    val b = f_1.-(a * index).%(size)

    println(s"a = $a, b = $b")

    val calculated_f_1 = a.*(index).+(b).%(size)
    val calculated_f_2 = a.*(calculated_f_1).+(b).%(size)
    val calculated_f_3 = a.*(calculated_f_2).+(b).%(size)

    println(s"f_1 = $f_1 = $calculated_f_1")
    println(s"f_2 = $f_2 = $calculated_f_2")
    println(s"f_3 = $f_3 = $calculated_f_3")

    //Now do it N times
    val n = BigInt(101741582076661L)

    val aToTheN = Day22Helpers.modPow(a, n, size)
    val aPart = aToTheN.*(index)
    val oneMinusAToTheN = BigInt(1).-(aToTheN)
    val oneMinusA = BigInt(1).-(a)
    val quotient = oneMinusAToTheN.*(Day22Helpers.moduloInverse(oneMinusA, size))
    val remainder = b.*(quotient)

    println(s"$a to the $n base $size = $aToTheN")


    aPart.+(remainder).%(size).+(size)
  }
}

object Day22Helpers {
  val cutRegex: Regex = """^cut (.*)$""".r
  val dealRegex: Regex = """^deal with increment (.*)$""".r

  def applyInstructions(instructions: List[String], size: BigInt, index: BigInt): BigInt = {
    var ret = index

    instructions.foreach { instruction =>
      ret = applyInstruction(ret, instruction, size)
    }

    ret
  }

  private def applyInstruction(index: BigInt, instruction: String, size: BigInt): BigInt = {
    if (instruction == "deal into new stack") {
      size - index - 1
    } else if (cutRegex.findAllMatchIn(instruction).nonEmpty) {
      val cutRegex(amount) = instruction
      applyCut(index, amount.toLong, size)
    } else if (dealRegex.findAllMatchIn(instruction).nonEmpty) {
      val dealRegex(increment) = instruction
      (index * increment.toLong) % size
    } else {
      println(s"UNHANDLED: $instruction")
      index
    }
  }

  private def applyCut(index: BigInt, amount: Long, size: BigInt): BigInt = {
    val actualCutAmount: BigInt = if (amount > 0) BigInt(amount) else size.+(amount)

    if (index.<(actualCutAmount)) {
      index + (size - actualCutAmount)
    } else {
      index - actualCutAmount
    }
  }

  /**
   * Play the shuffle instructions backwards
   */
  def applyInversedInstructions(instructions: List[String], size: BigInt, index: BigInt): BigInt = {
    var ret = index
    instructions.reverse.foreach { instruction =>
      ret = applyReversedInstruction(instruction, size, ret)
    }

    ret
  }
  private def applyReversedInstruction(instruction: String, size: BigInt, index: BigInt): BigInt = {
    if (instruction == "deal into new stack") {
      size.-(index).-(1)
    } else if (cutRegex.findAllMatchIn(instruction).nonEmpty) {
      val cutRegex(amount) = instruction
      val newAmount = -amount.toLong
      applyCut(index, newAmount, size)
    } else if (dealRegex.findAllMatchIn(instruction).nonEmpty) {
      val dealRegex(increment) = instruction
      val parsedIncrement = BigInt(increment)

      var newIx: BigInt = index
      while (newIx.%(parsedIncrement) != 0) {
        newIx = newIx.+(size)
      }

      newIx./(parsedIncrement)
    } else {
      println(s"UNHANDLED: $instruction")
      index
    }
  }

  //Need to implement modular power so we can do number^(base - 2) % base in a reasonable amount of time.
  def moduloInverse(number: BigInt, base: BigInt): BigInt = {
    val result = modPow(number, base - 2, base)

    println(s"Finding modulo inverse of $number in $base: $result")
    result
  }

  def modPow(number: BigInt, power: BigInt, base: BigInt): BigInt = {
    var result = BigInt(1L)
    var currentNumber = number.mod(base)
    var currentPower = power
    val zero = BigInt(0)
    while (currentPower.compareTo(zero) != 0) {
      if (currentPower.mod(2) > 0) {
        result = currentNumber.*(result).%(base)
      }
      currentNumber = currentNumber.*(currentNumber).%(base)
      currentPower = currentPower.>>(1) //bit shift right
    }

    result
  }
}
