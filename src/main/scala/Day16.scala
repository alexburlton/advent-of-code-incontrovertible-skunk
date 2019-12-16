import scala.collection.mutable.ListBuffer

class Day16 extends AbstractPuzzle(16) {
  val inputList: Vector[Int] = inputLines.head.split("").filter { _.nonEmpty }.map { _.toInt }.toVector
  val inputPattern: List[Int] = List(0, 1, 0, -1)
  val messageOffset: Int = 5977603

  override def partA(): Any = {
    var list = inputList
    for (_ <- 0 until 100) {
      list = runPhase(list)
    }

    list
  }

  private def runPhase(inputList: Vector[Int]): Vector[Int] = {
    val listBuffer = ListBuffer[Int]()

    for (i <- inputList.indices) {
      val phases = producePhaseList(i)

      val newDigit = calculateOutputDigit(phases, inputList)
      listBuffer.addOne(newDigit)
    }

    listBuffer.toVector
  }
  private def producePhaseList(index: Int): Vector[Int] = {
    val numberOfRepeats = index + 1

    Vector.fill(numberOfRepeats)(0) ++
      Vector.fill(numberOfRepeats)(1) ++
      Vector.fill(numberOfRepeats)(0) ++
      Vector.fill(numberOfRepeats)(-1)
  }
  private def calculateOutputDigit(phases: Vector[Int], inputs: Vector[Int]): Int = {
    val sum = inputs.zipWithIndex.map { case (item: Int, index: Int) =>
      val phaseIndex = (1 + index) % phases.size
      item * phases(phaseIndex)
    }.sum

    Math.abs(sum % 10)
  }

  override def partB(): Any = {
    val list = Seq.fill(10000)(inputList).flatten.toVector
    var neededDigits = list.slice(messageOffset, list.size - 1)

    for (_ <- 0 until 100) {
      neededDigits = runLinearPhase(neededDigits)
    }

    neededDigits.slice(0, 8)
  }

  def runLinearPhase(list: Vector[Int]): Vector[Int] = {
    val newList = ListBuffer[Int]()

    var sum = list.sum
    for (i <- list.indices) {
      if (i > 0) {
        sum -= list(i - 1)
      }

      newList.addOne(sum % 10)
    }

    newList.toVector
  }
}
