class DayFive extends AbstractPuzzle(5) {

  val inputCommands: List[Int] = inputLines.head.split(",").map(s => s.toInt).toList

  override def partA(): Int = {
    val computer = new IntcodeComputer(inputCommands, List(1))
    computer.process()

    println(computer.outputs)
    computer.outputs.last
  }

  override def partB(): Int = {
    val computer = new IntcodeComputer(inputCommands, List(5))
    computer.process()

    println(computer.outputs)
    computer.outputs.last
  }
}
