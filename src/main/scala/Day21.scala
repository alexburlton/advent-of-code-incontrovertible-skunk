class Day21 extends AbstractPuzzle(21) {
  val intcode: List[Long] = AdventUtils.parseAsIntcodeInput(inputLines)

  override def partA(): Any = {
    val lines = List("OR D T", "NOT C J", "AND T J", "NOT A T", "OR T J", "WALK")
    val inputs = lines.flatMap(AdventUtils.convertInstructionStringToASCIIInput)

    val computer = new IntcodeComputer(intcode, inputs)
    computer.process()

    computer.outputs.last
  }

  override def partB(): Any = {
    val lines = List("OR D J", "OR A T", "AND B T", "AND C T", "NOT T T", "AND T J", "NOT T T", "OR E T", "OR H T", "AND T J", "RUN")

    val inputs = lines.flatMap(AdventUtils.convertInstructionStringToASCIIInput)

    val computer = new IntcodeComputer(intcode, inputs)
    computer.process()

    computer.outputs.last
  }
}
