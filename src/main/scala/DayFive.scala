class DayFive extends AbstractPuzzle(5) {

  val inputCommands: List[Long] = inputLines.head.split(",").map(s => s.toLong).toList

  override def partA(): Long = {
    val computer = new IntcodeComputer(inputCommands, List(1))
    computer.process()

    println(computer.outputs)
    computer.outputs.last
  }

  override def partB(): Long = {
    val computer = new IntcodeComputer(inputCommands, List(5))
    computer.process()

    println(computer.outputs)
    computer.outputs.last
  }
}
