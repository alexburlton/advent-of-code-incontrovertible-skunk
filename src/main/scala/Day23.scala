import scala.collection.mutable.ListBuffer

class Day23 extends AbstractPuzzle(23) {
  val intcode: List[Long] = AdventUtils.parseAsIntcodeInput(inputLines)

  case class Message(address: Long, x: Long, y: Long)

  override def partA(): Any = {
    val range = 0 to 49
    val computers = range.map { i => new IntcodeComputer(intcode, List(i)) }.toList

    val messageQueue = ListBuffer[Message]()
    while (messageQueue.count { m => m.address == 255L } == 0) {
      processRound(computers, messageQueue)
    }

    messageQueue.find { m => m.address == 255L }
  }

  private def addOutputsToQueue(computer: IntcodeComputer, messageQueue: ListBuffer[Message]): Unit = {
    val messages = computer.outputs.grouped(3).map { it => Message(it(0), it(1), it(2)) }
    messageQueue.addAll(messages)
    computer.clearOutputs()
  }

  override def partB(): Any = {
    val range = 0 to 49
    val computers = range.map { i => new IntcodeComputer(intcode, List(i)) }.toList

    var previousNatInstructionDelivered = Message(0, -1L, -1000L)
    var latestNatInstructionDelievered = Message(0, -1L, -100L)

    val messageQueue = ListBuffer[Message]()

    while (previousNatInstructionDelivered.y != latestNatInstructionDelievered.y) {
      val natInstruction = processUntilIdle(computers, messageQueue)
      messageQueue.addOne(Message(0L, natInstruction.x, natInstruction.y))

      previousNatInstructionDelivered = latestNatInstructionDelievered
      latestNatInstructionDelievered = natInstruction
    }

    println(s"$previousNatInstructionDelivered, $latestNatInstructionDelievered")
    latestNatInstructionDelievered.y
  }

  private def processUntilIdle(computers: List[IntcodeComputer],
                               messageQueue: ListBuffer[Message]): Message = {
    processRound(computers, messageQueue)

    var lastNatInstruction: Message = Message(0L, -1, -1)

    while (messageQueue.nonEmpty) {
      val natInstruction: Option[Message] = messageQueue.findLast(_.address == 255L)
      lastNatInstruction = natInstruction.getOrElse(lastNatInstruction)
      removeAllNatMessages(messageQueue)

      processRound(computers, messageQueue)
    }

    println(s"IDLE, sending $lastNatInstruction")
    lastNatInstruction
  }

  private def processRound(computers: List[IntcodeComputer], messageQueue: ListBuffer[Message]): Unit = {
    computers.foreach { computer =>
      val nextMessage = messageQueue.find { m => m.address == computer.initialInputs.head }
      if (nextMessage.isDefined) {
        val message = nextMessage.get
        messageQueue.remove(messageQueue.indexOf(message))

        computer.processWithInput(message.x)
        computer.processWithInput(message.y)

        addOutputsToQueue(computer, messageQueue)
      } else {
        computer.processWithInput(-1)

        addOutputsToQueue(computer, messageQueue)
      }
    }
  }
  private def removeAllNatMessages(messageQueue: ListBuffer[Message]): Unit = {
    messageQueue.filterInPlace(_.address != 255L)
  }
}
