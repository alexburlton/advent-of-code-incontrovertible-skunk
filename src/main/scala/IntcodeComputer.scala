import scala.collection.mutable.ListBuffer

class IntcodeComputer(initialMemory: List[Int], private val initialInputs: List[Int] = List()) {
  private val memory: ListBuffer[Int] = initialMemory.to(ListBuffer)
  private val inputs: ListBuffer[Int] = initialInputs.to(ListBuffer)

  private var instructionPointer: Int = 0
  private var inputPointer: Int = 0
  var terminate = false

  private var waitingForInput = false

  val outputs: ListBuffer[Int] = new ListBuffer[Int]()

  def makeInitialSubstitution(noun: Int, verb: Int) {
    memory(1) = noun
    memory(2) = verb
  }

  def process(): List[Int] = {
    while (!terminate && !waitingForInput) {
      val opCode = readOpCode()
      opCode.process()
    }

    memory.toList
  }

  def processWithInput(input: Int): Unit = {
    waitingForInput = false
    inputs.addOne(input)
    process()
  }

  private def waitForInput(): Unit = {
    waitingForInput = true

    //Go back so we process the 03 op code again when the program is resumed
    instructionPointer -= 2
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
      case  3 => new OpCodeThree(parameterModes)
      case  4 => new OpCodeFour(parameterModes)
      case  5 => new OpCodeFive(parameterModes)
      case  6 => new OpCodeSix(parameterModes)
      case  7 => new OpCodeSeven(parameterModes)
      case  8 => new OpCodeEight(parameterModes)
    }
  }

  sealed abstract class OpCode(val paramCount: Int, rawModes: List[Int]) {
    protected val parameters: List[Int] = readParameters()
    private val parameterModes = readParameterModes(rawModes)

    def process(): Unit = {
      processImpl()
    }
    def processImpl()

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
    override def processImpl(): Unit = {
      terminate = true
    }
  }

  sealed class OpCodeOne(rawModes: List[Int]) extends OpCode(3, rawModes) {
    override def processImpl(): Unit = {
      memory(parameters(2)) = getParameter(0) + getParameter(1)
    }
  }

  sealed class OpCodeTwo(rawModes: List[Int]) extends OpCode(3, rawModes) {
    override def processImpl(): Unit = {
      memory(parameters(2)) = getParameter(0) * getParameter(1)
    }
  }

  sealed class OpCodeThree(rawModes: List[Int]) extends OpCode(1, rawModes) {
    override def processImpl(): Unit = {
      if (!inputs.indices.contains(inputPointer)) {
        waitForInput()
      } else {
        memory(parameters.head) = inputs(inputPointer)
        inputPointer += 1
      }
    }
  }

  sealed class OpCodeFour(rawModes: List[Int]) extends OpCode(1, rawModes) {
    override def processImpl(): Unit = {
      outputs.addOne(getParameter(0))
    }
  }

  sealed class OpCodeFive(rawModes: List[Int]) extends OpCode(2, rawModes) {
    override def processImpl(): Unit = {
      val param = getParameter(0)
      if (param != 0) {
        instructionPointer = getParameter(1)
      }
    }
  }

  sealed class OpCodeSix(rawModes: List[Int]) extends OpCode(2, rawModes) {
    override def processImpl(): Unit = {
      val param = getParameter(0)
      if (param == 0) {
        instructionPointer = getParameter(1)
      }
    }
  }

  sealed class OpCodeSeven(rawModes: List[Int]) extends OpCode(3, rawModes) {
    override def processImpl(): Unit = {
      val param = getParameter(0)
      val param2 = getParameter(1)
      memory(parameters(2)) = if (param < param2) 1 else 0
    }
  }

  sealed class OpCodeEight(rawModes: List[Int]) extends OpCode(3, rawModes) {
    override def processImpl(): Unit = {
      val param = getParameter(0)
      val param2 = getParameter(1)
      memory(parameters(2)) = if (param == param2) 1 else 0
    }
  }
}

