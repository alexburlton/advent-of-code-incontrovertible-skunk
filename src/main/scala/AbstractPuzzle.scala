abstract class AbstractPuzzle(day: Int) {
  val inputLines: List[String] = AdventUtils.readFile(s"inputs/Day$day")

  def run(): Unit = {
    println(s"${day}A: ${partA()}")
    println(s"${day}B: ${partB()}")
  }

  def partA(): Any
  def partB(): Any
}
