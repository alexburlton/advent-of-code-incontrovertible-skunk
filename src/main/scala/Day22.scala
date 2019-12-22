import scala.collection.mutable
import scala.util.matching.Regex

class Day22 extends AbstractPuzzle(22)
{
  val cutRegex: Regex = """^cut (.*)$""".r
  val dealRegex: Regex = """^deal with increment (.*)$""".r

  override def partA(): Any = {
    val size = 10007L
    var index = 2019L

    inputLines.foreach { instruction =>
      index = getNewIndex(index, instruction, size)
    }

    index
  }

  private def getNewIndex(index: Long, instruction: String, size: Long): Long = {
    if (instruction == "deal into new stack") {
      size - index - 1
    } else if (cutRegex.findAllMatchIn(instruction).nonEmpty) {
      val cutRegex(amount) = instruction
      applyCut(index, amount.toInt, size)
    } else if (dealRegex.findAllMatchIn(instruction).nonEmpty) {
      val dealRegex(increment) = instruction
      (index * increment.toInt) % size
    } else {
      println(s"UNHANDLED: $instruction")
      index
    }
  }

  private def applyCut(index: Long, amount: Int, size: Long): Long = {
    val actualCutAmount = if (amount > 0) amount else size + amount

    if (index < actualCutAmount) {
      index + (size - actualCutAmount)
    } else {
      index - actualCutAmount
    }
  }

  override def partB(): Any = {
    val size = 119315717514047L
    val index = 2020L

    //want to find a single expression for applying the inverted list once.
    val f_1 = applyInversedInstructions(inputLines, size, index)
    val f_2 = applyInversedInstructions(inputLines, size, f_1)

    val a_numerator = f_1 - f_2
    val a_denominator = index - f_1 + size

    println(s"Want multiplicative inverse of $a_denominator mod $size")

    //WTF...
    val a = (27832204065150L * a_numerator) % size
    val b = (f_1 - (a * 2020)) % size

    println(s"a = $a, b = $b")

    val calculated_f_1 = size + (a * index + b) % size
    val calculated_f_2 = ((a * calculated_f_1) + b) % size

    println(s"f_1 = ${f_1} = $calculated_f_1")
    println(s"f_2 = ${f_2} = $calculated_f_2")

    index
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

  private def applyInversedInstructions(instructions: List[String], size: Long, index: Long): Long = {
    var ret = index
    inputLines.reverse.foreach { instruction =>
      ret = applyReversedInstruction(instruction, size, ret)
    }

    ret
  }


  private def applyReversedInstruction(instruction: String, size: Long, index: Long): Long = {
    if (instruction == "deal into new stack") {
      size - index - 1
    } else if (cutRegex.findAllMatchIn(instruction).nonEmpty) {
      val cutRegex(amount) = instruction
      val newAmount = -amount.toInt
      applyCut(index, newAmount, size)
    } else if (dealRegex.findAllMatchIn(instruction).nonEmpty) {
      val dealRegex(increment) = instruction
      var newIx: Long = index
      while (newIx % increment.toInt != 0) {
        newIx += size
      }

      newIx / increment.toInt
    } else {
      println(s"UNHANDLED: $instruction")
      index
    }
  }
}
