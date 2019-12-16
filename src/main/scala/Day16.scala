import scala.collection.mutable.ListBuffer

class Day16 extends AbstractPuzzle(16) {
  val inputList: Vector[Int] = Day16Helpers.getDigitList(inputLines.head)
  val inputPattern: List[Int] = List(0, 1, 0, -1)
  val messageOffset: Int = 5977603

  override def partA(): Any = {
    Day16Helpers.runPhases(inputLines.head, 100)
  }

  override def partB(): Any = {
    Day16Helpers.runPhasesWithOffset(inputLines.head, 100)
  }
}

object Day16Helpers {
  def getDigitList(digits: String): Vector[Int] = {
    digits.split("").filter { _.nonEmpty }.map { _.toInt }.toVector
  }

  def runPhases(digits: String, times: Int): String ={
    var inputs = getDigitList(digits)

    for (_ <- 0 until times) {
      inputs = runPhase(inputs)
    }

    inputs.slice(0, 8).mkString
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

  def runPhasesWithOffset(digitStr: String, times: Int): String = {
    val digits = getDigitList(digitStr)
    val list = Seq.fill(10000)(digits).flatten.toVector
    val messageOffset = digitStr.slice(0, 7).mkString.toInt
    var neededDigits = list.slice(messageOffset, list.size - 1)

    for (_ <- 0 until 100) {
      neededDigits = runLinearPhase(neededDigits)
    }

    neededDigits.slice(0, 8).mkString
  }
  private def runLinearPhase(list: Vector[Int]): Vector[Int] = {
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
