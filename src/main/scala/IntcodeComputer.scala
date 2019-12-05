import scala.collection.mutable.ListBuffer

class IntcodeComputer(initialMemory: List[Int]) {
  private val memory: ListBuffer[Int] = initialiseMemory(initialMemory)
  private var instructionPointer: Int = 0
  private var terminate = false

  private def initialiseMemory(initialMemory: List[Int]): ListBuffer[Int] = {
    val list = new ListBuffer[Int]()
    list.addAll(initialMemory)
    list
  }

  def makeInitialSubstitution(noun: Int, verb: Int) {
    memory(1) = noun
    memory(2) = verb
  }

  def process(): List[Int] = {
    while (!terminate) {
      val instruction = memory(instructionPointer)
      if (instruction == 99) {
        terminate = true
      } else {
        processNonTerminatingCommand(instruction)
      }
    }

    memory.toList
  }

  private def processNonTerminatingCommand(instruction: Int): Unit = {
    val paramOne = memory(instructionPointer + 1)
    val paramTwo = memory(instructionPointer + 2)
    val paramThree = memory(instructionPointer + 3)

    if (instruction == 1) {
      memory(paramThree) = memory(paramOne) + memory(paramTwo)
    } else {
      memory(paramThree) = memory(paramOne) * memory(paramTwo)
    }

    instructionPointer += 4
  }
}
