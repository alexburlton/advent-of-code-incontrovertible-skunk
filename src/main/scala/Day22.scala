import scala.collection.mutable
import scala.util.matching.Regex

class Day22 extends AbstractPuzzle(22)
{
  val cutRegex: Regex = """^cut (.*)$""".r
  val dealRegex: Regex = """^deal with increment (.*)$""".r

  override def partA(): Any = {
    val size = 10007L
    val index = 2019L

    applyInstructions(inputLines, size, index)
  }

  private def getNewIndex(index: Long, instruction: String, size: Long): Long = {
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

  private def applyCut(index: Long, amount: Long, size: Long): Long = {
    val actualCutAmount = if (amount > 0) amount else size + amount

    if (index < actualCutAmount) {
      index + (size - actualCutAmount)
    } else {
      index - actualCutAmount
    }
  }

  override def partB(): Any = {
    //println(moduloInverse(4507L, 10007L))
    //println(getLinearEquationMultiplier(10007L))

    //val pow = modPow(3, 10005, 10007)
    //println(s"$pow")

    val size = 119315717514047L
    val index = 2019L

    val result = applyInstructions(inputLines, size, index)
    val result2 = applyInstructions(inputLines, size, result)

    val inverted1 = applyInversedInstructions(inputLines, size, result2)
    val inverted = applyInversedInstructions(inputLines, size, inverted1)

    println(s"$index -> $result -> $result2 -> $inverted1 -> $inverted")

    //want to find a single expression for applying the inverted list once.
    val otherA = getLinearEquationMultiplier(size)
    println(s"$otherA")

    val f_1 = applyInversedInstructions(inputLines, size, index)
    val f_2 = applyInversedInstructions(inputLines, size, f_1)
    val f_3 = applyInversedInstructions(inputLines, size, f_2)

    val a_numerator = positiveMod(f_1 - f_2, size)
    val a_denominator = positiveMod(index - f_1, size)

    println(s"Want multiplicative inverse of $a_denominator mod $size")
    val inverse = moduloInverse(a_denominator, size)
    //online calculators give 11164973111437 (verified via two methods...)

    //So this should work...
    val a = otherA
    val b = positiveMod(f_1 - (a * index), size)

    println(s"a = $a, b = $b")

    val calculated_f_1 = positiveMod(a * index + b, size)
    val calculated_f_2 = positiveMod((a * calculated_f_1) + b, size)
    val calculated_f_3 = positiveMod((a * calculated_f_2) + b, size)

    println(s"f_1 = $f_1 = $calculated_f_1")
    println(s"f_2 = $f_2 = $calculated_f_2")
    println(s"f_3 = $f_3 = $calculated_f_3")

    index
  }
  def getLinearEquationMultiplier(size: Long): Long = {
    var ret = 1L

    inputLines.reverse.foreach { instruction =>
      ret = applyReversedInstructionToMultiplier(instruction, size, ret)
      ret = ret % size
    }

    ret
  }

  private def positiveMod(number: Long, base: Long): Long = {
    var result = number % base
    while (result < 0) result += base

    if (result == 0) base else result
  }

//  private def modPow(number: Long, pow: Long, base: Long): Long = {
//    var done = 0
//    var result = number
//    while (done < pow) {
//      result = (result * pow) % base
//      println(s"$result")
//      done += 1
//    }
//
//    result
//  }

  private def applyReversedInstructionToMultiplier(instruction: String, size: Long, multiplier: Long): Long = {
    if (instruction == "deal into new stack") {
      multiplier * -1
    } else if (cutRegex.findAllMatchIn(instruction).nonEmpty) {
      //No effect
      multiplier
    } else if (dealRegex.findAllMatchIn(instruction).nonEmpty) {
      val dealRegex(increment) = instruction
      multiplier * moduloInverse(increment.toLong, size)
    } else {
      println(s"UNHANDLED: $instruction")
      multiplier
    }
  }

  private def applyInstructions(instructions: List[String], size: Long, index: Long): Long = {
    var ret = index

    inputLines.foreach { instruction =>
      ret = getNewIndex(ret, instruction, size)
    }

    ret
  }
  private def applyInversedInstructions(instructions: List[String], size: Long, index: Long): Long = {
    var ret = index
    inputLines.reverse.foreach { instruction =>
      ret = applyReversedInstruction(instruction, size, ret)
    }

    ret
  }

  //Need to implement modular power so we can do number^(base - 2) % base in a reasonable amount of time.
  private def moduloInverse(number: Long, base: Long): Long = {
    val result = modPow(number, base - 2, base)

    println(s"Finding modulo inverse of $number in $base: $result")
    result
//    var multiplier = 1
//    while (number * multiplier % base != 1) {
//      multiplier += 1
//    }
//
//    multiplier
  }

  private def modPow(number: Long, power: Long, base: Long): Long = {
    var result = 1L
    var currentNumber = number % base
    var currentPower = power
    while (currentPower > 0) {
      if (currentPower % 2 > 0) {
        result = positiveMod(result * currentNumber, base)
      }
      currentNumber = positiveMod(currentNumber * currentNumber, base)
      currentPower = currentPower >> 1 //bit shift right
    }

    result
  }

  private def applyReversedInstruction(instruction: String, size: Long, index: Long): Long = {
    if (instruction == "deal into new stack") {
      size - index - 1
    } else if (cutRegex.findAllMatchIn(instruction).nonEmpty) {
      val cutRegex(amount) = instruction
      val newAmount = -amount.toLong
      applyCut(index, newAmount, size)
    } else if (dealRegex.findAllMatchIn(instruction).nonEmpty) {
      val dealRegex(increment) = instruction

      var newIx: Long = index
      while (newIx % increment.toLong != 0) {
        newIx += size
      }

      newIx / increment.toLong
    } else {
      println(s"UNHANDLED: $instruction")
      index
    }
  }
}
