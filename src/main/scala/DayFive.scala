class DayFive extends AbstractPuzzle(5) {

  val inputCommands: List[Long] = AdventUtils.parseAsIntcodeInput(inputLines)

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
