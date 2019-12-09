class DaySeven extends AbstractPuzzle(7) {
  val inputCommands: List[Long] = inputLines.head.split(",").map(s => s.toLong).toList

  override def partA(): Long = DaySevenHelpers.calculateMaxThrusterSignal(inputCommands)

  override def partB(): Long = DaySevenHelpers.calculateMaxThrusterSignalWithFeedbackLoop(inputCommands)
}

object DaySevenHelpers {
  def calculateMaxThrusterSignal(initialMemory: List[Long]): Long = {
    val phaseSettingSequences = List(0, 1, 2, 3, 4).permutations
    phaseSettingSequences.map { sequence => calculateThrusterSignal(initialMemory, sequence) }.max
  }

  def calculateMaxThrusterSignalWithFeedbackLoop(initialMemory: List[Long]): Long = {
    val phaseSettingSequences = List(5, 6, 7, 8, 9).permutations
    phaseSettingSequences.map { sequence => calculateThrusterSignal(initialMemory, sequence) }.max
  }

  def calculateThrusterSignal(initialMemory: List[Long], phaseSettingSequence: List[Int]): Long = {
    var previousOutput: Long = 0

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
