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
      val opCode = readOpCode()
      opCode.process()
    }

    memory.toList
  }

  private def readOpCode(): OpCode = {
    val instruction = memory(instructionPointer)

    instruction match {
      case 99 => new OpCodeTerminate()
      case  1 => new OpCodeOne()
      case  2 => new OpCodeTwo()
    }
  }

  sealed abstract class OpCode(val expectedParameters: Int) {
    protected val parameters: List[Int] = readParameters()

    def process()

    private def readParameters(): List[Int] = {
      val buffer = new ListBuffer[Int]()
      for (_ <- 0 until expectedParameters) {
        instructionPointer += 1
        buffer.addOne(memory(instructionPointer))
      }

      buffer.toList
    }
  }

  class OpCodeTerminate extends OpCode(0) {
    override def process(): Unit = {
      terminate = true
    }
  }

  class OpCodeOne extends OpCode(4) {
    override def process(): Unit = {
      memory(parameters(2)) = memory(parameters(0)) + memory(parameters(1))
    }
  }

  class OpCodeTwo extends OpCode(4) {
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
      println(memory(parameters.head))
    }
  }
}

