class DayNine extends AbstractPuzzle(9) {
  val inputCommands: List[Long] = inputLines.head.split(",").map(s => s.toLong).toList

  override def partA(): Long = {
    val computer = new IntcodeComputer(inputCommands, List(1))
    computer.process()

    computer.outputs.head
  }

  override def partB(): Long = {
    val computer = new IntcodeComputer(inputCommands, List(2))
    computer.process()

    computer.outputs.head
  }
}
