class DayFive extends AbstractPuzzle(5) {

  val inputCommands: List[Int] = inputLines.head.split(",").map(s => s.toInt).toList

  override def partA(): Int = {
    val computer = new IntcodeComputer(inputCommands)
    computer.process()

    println(computer.outputs)
    computer.outputs.last
  }

  override def partB(): Int = -1
}
