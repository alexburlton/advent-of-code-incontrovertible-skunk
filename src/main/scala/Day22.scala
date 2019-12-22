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

    val (a, b) = Day22Helpers.constructPolynomialForReverse(inputLines, size)

    //Now apply the polynomial N times
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
    if (instruction == "deal into new stack")
    {
      size - index - 1
    }
    else if (cutRegex.findAllMatchIn(instruction).nonEmpty)
    {
      val cutRegex(amount) = instruction
      val newAmount = -amount.toLong
      applyCut(index, newAmount, size)
    }
    else if (dealRegex.findAllMatchIn(instruction).nonEmpty)
    {
      val dealRegex(incrementStr) = instruction
      val increment = BigInt(incrementStr)

      var newIx: BigInt = index
      while (newIx % increment != 0) {
        newIx = newIx + size
      }

      newIx / increment
    }
    else
    {
      println(s"UNHANDLED: $instruction")
      index
    }
  }

  /**
   * Returns (a, b), where the reversed instructions correspond to ax + b
   */
  def constructPolynomialForReverse(instructions: List[String], size: BigInt): (BigInt, BigInt) = {
    //Pick an arbitrary element that's in range
    val index = size./(3)

    //Calculate f(index) and f(f(index))
    val f_1 = Day22Helpers.applyInversedInstructions(instructions, size, index)
    val f_2 = Day22Helpers.applyInversedInstructions(instructions, size, f_1)

    //a = f - f(f) / x - f, where / is "modulo division", i.e. times by modulo inverse
    val a_numerator = f_1.-(f_2).%(size)
    val a_denominator = index.-(f_1).%(size)
    val inverse = Day22Helpers.moduloInverse(a_denominator, size)
    val a = a_numerator.*(inverse).%(size)

    //Now can compute b, as f(x) = ax + b => b = f(x) - ax
    val b = f_1.-(a * index).%(size)

    (a, b)
  }

  /**
   * Use Euler's theorem to calculate modular inverse quickly
   *
   * https://en.wikipedia.org/wiki/Modular_multiplicative_inverse#Using_Euler's_theorem
   */
  def moduloInverse(number: BigInt, base: BigInt): BigInt = {
    val result = modPow(number, base - 2, base)
    result
  }

  /**
   * Modular power method - that comes out of the box in Python... :(
   * https://en.wikipedia.org/wiki/Modular_exponentiation#Right-to-left_binary_method
   */
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
