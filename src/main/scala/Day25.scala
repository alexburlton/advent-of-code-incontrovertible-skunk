import java.util.Scanner

class Day25 extends AbstractPuzzle(25) {
  val intcode: List[Long] = AdventUtils.parseAsIntcodeInput(inputLines)
  val computer = new IntcodeComputer(intcode)

  override def partA(): Any = {
    computer.process()

    println(computer.outputs.map(_.asInstanceOf[Char]).mkString)

    val in = new Scanner(System.in)
    while (true) {
      val str = in.nextLine()
      val command = AdventUtils.convertInstructionStringToASCIIInput(str)

      computer.processWithInputs(command)
      println(computer.outputs.map(_.asInstanceOf[Char]).mkString)
    }
  }

  override def partB(): Any = -1
}
