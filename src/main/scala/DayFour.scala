import scala.util.matching.Regex

class DayFour extends AbstractPuzzle(4) {

  override def partA(): Int = {
    val range = readRange()

    val result = range.filter(DayFourHelpers.validPasswordPartA)
    result.size
  }

  override def partB(): Int = {
    val range = readRange()

    val result = range.filter(DayFourHelpers.validPasswordPartB)
    result.size
  }

  private def readRange(): Range = {
    val line = inputLines.head
    val lowerBound = line.split("-").head.toInt
    val upperBound = line.split("-")(1).toInt

    lowerBound to upperBound
  }
}

object DayFourHelpers {
  def hasRepeatedDigits(number: Int): Boolean = {
    val regex = """(\d)\1""".r
    regex.findAllMatchIn(number.toString).nonEmpty
  }

  /**
   * Match against a regex like (?<!0)0{2}(?!0) for each digit 0-9
   * 0{2} matches on two zeros
   * (?<!0) is a negative look-behind for 0
   * (?!0) is a negative look-ahead for 0
   */
  val exactPairRegexes: List[Regex] = (0 to 9).map { digit => s"(?<!$digit)$digit{2}(?!$digit)".r }.toList
  def hasExactPair(number: Int): Boolean = exactPairRegexes.exists(_.findAllMatchIn(number.toString).nonEmpty)

  def digitsAreNotDecreasing(number: Int): Boolean = {
    val list = number.toString.toList

    var current = list.head.toInt
    list.foreach { str =>
      val digit = str.toInt
      if (digit < current) return false
      current = digit
    }

    true
  }

  def validPasswordPartA(num: Int): Boolean = hasRepeatedDigits(num) && digitsAreNotDecreasing(num)
  def validPasswordPartB(num: Int): Boolean = hasExactPair(num) && digitsAreNotDecreasing(num)
}
