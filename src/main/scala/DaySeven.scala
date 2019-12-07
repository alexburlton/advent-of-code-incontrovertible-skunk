class DaySeven extends AbstractPuzzle(7) {
  val inputCommands: List[Int] = inputLines.head.split(",").map(s => s.toInt).toList

  override def partA(): Int = DaySevenHelpers.calculateMaxThrusterSignal(inputCommands)

  override def partB(): Int = DaySevenHelpers.calculateMaxThrusterSignalWithFeedbackLoop(inputCommands)
}

object DaySevenHelpers {
  def calculateMaxThrusterSignal(initialMemory: List[Int]): Int = {
    val phaseSettingSequences = List(0, 1, 2, 3, 4).permutations
    phaseSettingSequences.map { sequence => calculateThrusterSignal(initialMemory, sequence) }.max
  }

  def calculateMaxThrusterSignalWithFeedbackLoop(initialMemory: List[Int]): Int = {
    val phaseSettingSequences = List(5, 6, 7, 8, 9).permutations
    phaseSettingSequences.map { sequence => calculateThrusterSignal(initialMemory, sequence) }.max
  }

  def calculateThrusterSignal(initialMemory: List[Int], phaseSettingSequence: List[Int]): Int = {
    var previousOutput = 0

    val amplifiers = phaseSettingSequence.map { setting => new IntcodeComputer(initialMemory, List(setting)) }
    while (!amplifiers.head.terminate) {
      amplifiers.foreach { amplifier =>
        amplifier.processWithInput(previousOutput)
        previousOutput = amplifier.outputs.last
      }
    }

    previousOutput
  }
}
