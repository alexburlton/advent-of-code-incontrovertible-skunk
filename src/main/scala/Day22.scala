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
    val size = 10007L
    var index = 6526L

    inputLines.reverse.foreach { instruction =>
      index = applyReversedInstruction(instruction, size, index)
    }

    index
  }
//  private def invertInstructions(size: Long): List[String] = {
//    inputLines.reverse.map { it => invertInstruction(it, size) }
//  }
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
