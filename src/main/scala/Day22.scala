class Day22 extends AbstractPuzzle(22)
{
  override def partA(): Any = {
    var cards = (0L to 10006L).toVector

    inputLines.foreach { instruction =>
      cards = getNewOrder(cards, instruction)
      //println(cards)
    }

    cards.indexOf(2019)
  }

  private def getNewOrder(cards: Vector[Long], instruction: String): Vector[Long] = {
    val cutRegex = """^cut (.*)$""".r
    val dealRegex = """^deal with increment (.*)$""".r

    if (instruction == "deal into new stack") {
      cards.reverse
    } else if (cutRegex.findAllMatchIn(instruction).nonEmpty) {
      val cutRegex(amount) = instruction
      applyCut(cards, amount.toInt)
    } else if (dealRegex.findAllMatchIn(instruction).nonEmpty) {
      val dealRegex(increment) = instruction
      applyDealWithIncrement(cards, increment.toInt)
    }else {
      println(s"UNHANDLED: $instruction")
      cards
    }
  }

  private def applyCut(cards: Vector[Long], amount: Int): Vector[Long] = {
    val actualCutAmount = if (amount > 0) amount else cards.length + amount

    val topStack = cards.slice(0, actualCutAmount)
    val bottomStack = cards.slice(actualCutAmount, cards.length)

    bottomStack ++ topStack
  }

  private def applyDealWithIncrement(cards: Vector[Long], increment: Int): Vector[Long] = {
    cards.zipWithIndex.sortBy { case (_: Long, index: Int) => getNewIndex(index, increment, cards) }
      .map[Long](_._1)
  }
  private def getNewIndex(index: Long, increment: Int, cards: Vector[Long]): Long = {
    (index * increment) % cards.length
  }

  override def partB(): Any = {
    var cards = (0L to 10006L).toVector

    inputLines.foreach { instruction =>
      cards = getNewOrder(cards, instruction)
      //println(cards)
    }

    cards.indexOf(2019)
  }
}
