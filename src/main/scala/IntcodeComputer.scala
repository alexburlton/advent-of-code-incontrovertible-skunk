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

    val digits = instruction.toString.map(_.asDigit)
    val parameterModes = digits.slice(0, digits.size - 2).toList

    opCode match {
      case 99 => new OpCodeTerminate()
      case  1 => new OpCodeOne(parameterModes)
      case  2 => new OpCodeTwo(parameterModes)
      case  3 => new OpCodeThree(input, parameterModes)
      case  4 => new OpCodeFour(parameterModes)
    }
  }

  sealed abstract class OpCode(val paramCount: Int, rawModes: List[Int]) {
    protected val parameters: List[Int] = readParameters()
    private val parameterModes = readParameterModes(rawModes)

    def process()

    def getParameter(index: Int): Int = {
      val mode = parameterModes(index)

      if (mode == 1) parameters(index)
      else memory(parameters(index))
    }


    private def readParameters(): List[Int] = {
      val buffer = new ListBuffer[Int]()
      for (_ <- 0 until paramCount) {
        buffer.addOne(memory(instructionPointer))
        instructionPointer += 1
      }

      buffer.toList
    }

    private def readParameterModes(rawModes: List[Int]): List[Int] = {
      val parsed = ListBuffer[Int]()
      parsed.addAll(rawModes.reverse)

      while (parsed.size < paramCount) {
        parsed.addOne(0)
      }

      parsed.toList
    }
  }

  sealed class OpCodeTerminate extends OpCode(0, List()) {
    override def process(): Unit = {
      terminate = true
    }
  }

  sealed class OpCodeOne(rawModes: List[Int]) extends OpCode(3, rawModes) {
    override def process(): Unit = {
      memory(parameters(2)) = getParameter(0) + getParameter(1)
    }
  }

  sealed class OpCodeTwo(rawModes: List[Int]) extends OpCode(3, rawModes) {
    override def process(): Unit = {
      memory(parameters(2)) = getParameter(0) * getParameter(1)
    }
  }

  sealed class OpCodeThree(val input: Int, rawModes: List[Int]) extends OpCode(1, rawModes) {
    override def process(): Unit = {
      memory(parameters.head) = input
    }
  }

  sealed class OpCodeFour(rawModes: List[Int]) extends OpCode(1, rawModes) {
    override def process(): Unit = {
      outputs.addOne(memory(parameters.head))
    }
  }
}

