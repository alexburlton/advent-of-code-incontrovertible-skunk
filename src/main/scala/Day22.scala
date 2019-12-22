class Day22 extends AbstractPuzzle(22)
{
  override def partA(): Any = {
    val size = 10007L
    var index = 2019L

    inputLines.foreach { instruction =>
      index = getNewIndex(index, instruction, size)
    }

    index
  }

  private def getNewIndex(index: Long, instruction: String, size: Long): Long = {
    val cutRegex = """^cut (.*)$""".r
    val dealRegex = """^deal with increment (.*)$""".r

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

  override def partB(): Any = -1
}
