import scala.collection.mutable.ListBuffer

class IntcodeComputer(initialMemory: List[Long], val initialInputs: List[Long] = List()) {
  private val memory: ListBuffer[Long] = initialMemory.to(ListBuffer)
  private val inputs: ListBuffer[Long] = initialInputs.to(ListBuffer)

  private var instructionPointer: Long = 0
  private var inputPointer: Int = 0
  var terminate = false

  private var waitingForInput = false
  private var relativeBase: Int = 0

  val outputs: ListBuffer[Long] = new ListBuffer[Long]()

  def copy(): IntcodeComputer = {
    val newComputer = new IntcodeComputer(memory.toList, inputs.toList)
    newComputer.instructionPointer = this.instructionPointer
    newComputer.inputPointer = this.inputPointer
    newComputer.terminate = this.terminate
    newComputer.waitingForInput = this.waitingForInput
    newComputer.relativeBase = this.relativeBase
    newComputer.outputs.addAll(this.outputs)

    newComputer
  }

  private def readMemory(index: Int): Long = {
    if (index >= memory.size) {
      padMemory(index)
    }

    memory(index)
  }

  def clearOutputs(): Unit = {
    outputs.clear()
  }

  private def writeMemory(index: Int, valueToWrite: Long): Unit = {
    if (index >= memory.size) {
      padMemory(index)
    }

    memory(index) = valueToWrite
  }

  private def padMemory(index: Int) = {
    val difference = index - memory.size + 1

    val zeros = List.fill(difference)(0L)
    memory.addAll(zeros)
  }

  def substituteValue(position: Int, value: Int): Unit = {
    memory(position) = value
  }

  def makeInitialSubstitution(noun: Int, verb: Int) {
    memory(1) = noun
    memory(2) = verb
  }

  def process(): List[Long] = {
    while (!terminate && !waitingForInput) {
      val opCode = readOpCode()
      opCode.process()
    }

    memory.toList
  }

  def processWithInput(input: Long): Unit = {
    waitingForInput = false
    inputs.addOne(input)
    process()
  }

  def processWithInputs(newInputs: List[Long]): Unit = {
    waitingForInput = false
    inputs.addAll(newInputs)
    process()
  }

  private def waitForInput(): Unit = {
    waitingForInput = true

    //Go back so we process the 03 op code again when the program is resumed
    instructionPointer -= 2
  }

  private def readOpCode(): OpCode = {
    val instruction = readMemory(instructionPointer.toInt)
    instructionPointer += 1

    val code = instruction % 100

    val digits = instruction.toString.map(_.asDigit)
    val parameterModes = digits.slice(0, digits.size - 2).toList

    val opCode = code match {
      case 99 => new OpCodeTerminate()
      case  1 => new OpCodeOne()
      case  2 => new OpCodeTwo()
      case  3 => new OpCodeThree()
      case  4 => new OpCodeFour()
      case  5 => new OpCodeFive()
      case  6 => new OpCodeSix()
      case  7 => new OpCodeSeven()
      case  8 => new OpCodeEight()
      case  9 => new OpCodeNine()
    }

    opCode.readParameterModes(parameterModes)
    opCode
  }

  sealed abstract class OpCode(val paramCount: Int) {
    protected val parameters: List[Long] = readParameters()
    private val parameterModes = ListBuffer[Int]()

    def process(): Unit = {
      processImpl()
    }
    def processImpl()

    def getParameter(index: Int): Long = {
      val mode = parameterModes(index)

      if (mode == 1) parameters(index)
      else if (mode == 2) readMemory(parameters(index).toInt + relativeBase)
      else readMemory(parameters(index).toInt)
    }

    def getPositionForWriting(index: Int): Int = {
      val mode = parameterModes(index)

      if (mode == 2) parameters(index).toInt + relativeBase
      else parameters(index).toInt
    }

    private def readParameters(): List[Long] = {
      val buffer = new ListBuffer[Long]()
      for (_ <- 0 until paramCount) {
        buffer.addOne(readMemory(instructionPointer.toInt))
        instructionPointer += 1
      }

      buffer.toList
    }

    def readParameterModes(rawModes: List[Int]) {
      parameterModes.addAll(rawModes.reverse)

      while (parameterModes.size < paramCount) {
        parameterModes.addOne(0)
      }
    }
  }

  sealed class OpCodeTerminate extends OpCode(0) {
    override def processImpl(): Unit = {
      terminate = true
    }
  }

  sealed class OpCodeOne() extends OpCode(3) {
    override def processImpl(): Unit = {
      writeMemory(getPositionForWriting(2), getParameter(0) + getParameter(1))
    }
  }

  sealed class OpCodeTwo() extends OpCode(3) {
    override def processImpl(): Unit = {
      writeMemory(getPositionForWriting(2), getParameter(0) * getParameter(1))
    }
  }

  sealed class OpCodeThree() extends OpCode(1) {
    override def processImpl(): Unit = {
      if (!inputs.indices.contains(inputPointer)) {
        waitForInput()
      } else {
        writeMemory(getPositionForWriting(0), inputs(inputPointer))
        inputPointer += 1
      }
    }
  }

  sealed class OpCodeFour() extends OpCode(1) {
    override def processImpl(): Unit = {
      outputs.addOne(getParameter(0))
    }
  }

  sealed class OpCodeFive() extends OpCode(2) {
    override def processImpl(): Unit = {
      val param = getParameter(0)
      if (param != 0) {
        instructionPointer = getParameter(1)
      }
    }
  }

  sealed class OpCodeSix() extends OpCode(2) {
    override def processImpl(): Unit = {
      val param = getParameter(0)
      if (param == 0) {
        instructionPointer = getParameter(1)
      }
    }
  }

  sealed class OpCodeSeven() extends OpCode(3) {
    override def processImpl(): Unit = {
      val param = getParameter(0)
      val param2 = getParameter(1)
      writeMemory(getPositionForWriting(2), if (param < param2) 1 else 0)
    }
  }

  sealed class OpCodeEight() extends OpCode(3) {
    override def processImpl(): Unit = {
      val param = getParameter(0)
      val param2 = getParameter(1)
      writeMemory(getPositionForWriting(2), if (param == param2) 1 else 0)
    }
  }

  sealed class OpCodeNine() extends OpCode(1) {
    override def processImpl(): Unit = {
      relativeBase += getParameter(0).toInt
    }
  }
}

