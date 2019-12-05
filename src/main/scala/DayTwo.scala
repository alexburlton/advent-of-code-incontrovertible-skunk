

class DayTwo extends AbstractPuzzle(2) {

  val inputCommands: List[Int] = inputLines.head.split(",").map(s => s.toInt).toList

  override def partA(): Int = {
    val computer = new IntcodeComputer(inputCommands)
    computer.makeInitialSubstitution(12, 2)
    computer.process().head
  }

  override def partB(): Int = findNounAndVerbForOutput(inputCommands, 19690720)

  def findNounAndVerbForOutput(commands: List[Int], output: Int): Int = {
    for (noun <- 0 to 99) {
      for (verb <- 0 to 99) {
        val computer = new IntcodeComputer(inputCommands)
        computer.makeInitialSubstitution(noun, verb)
        val result = computer.process().head
        if (result == output) {
          return 100 * noun + verb
        }
      }
    }

    -1
  }
}
