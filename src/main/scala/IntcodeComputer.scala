import scala.collection.mutable.ListBuffer

class IntcodeComputer(initialMemory: List[Int], private val input: Int = 1) {
  private val memory: ListBuffer[Int] = initialiseMemory(initialMemory)
  private var instructionPointer: Int = 0
  private var terminate = false
  val outputs: ListBuffer[Int] = new ListBuffer[Int]()

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
      val opCode = readOpCode()
      opCode.process()
    }

    memory.toList
  }

  private def readOpCode(): OpCode = {
    val instruction = memory(instructionPointer)
    instructionPointer += 1

    val opCode = instruction % 100

    opCode match {
      case 99 => new OpCodeTerminate()
      case  1 => new OpCodeOne()
      case  2 => new OpCodeTwo()
      case  3 => new OpCodeThree(input)
      case  4 => new OpCodeFour()
    }
  }

  sealed abstract class OpCode(val expectedParameters: Int) {
    protected val parameters: List[Int] = readParameters()

    def process()

    private def readParameters(): List[Int] = {
      val buffer = new ListBuffer[Int]()
      for (_ <- 0 until expectedParameters) {
        buffer.addOne(memory(instructionPointer))
        instructionPointer += 1
      }

      buffer.toList
    }
  }

  class OpCodeTerminate extends OpCode(0) {
    override def process(): Unit = {
      terminate = true
    }
  }

  class OpCodeOne extends OpCode(3) {
    override def process(): Unit = {
      memory(parameters(2)) = memory(parameters(0)) + memory(parameters(1))
    }
  }

  class OpCodeTwo extends OpCode(3) {
    override def process(): Unit = {
      memory(parameters(2)) = memory(parameters(0)) * memory(parameters(1))
    }
  }

  class OpCodeThree(val input: Int) extends OpCode(1) {
    override def process(): Unit = {
      memory(parameters.head) = input
    }
  }

  class OpCodeFour() extends OpCode(1) {
    override def process(): Unit = {
      outputs.addOne(memory(parameters.head))
    }
  }
}

