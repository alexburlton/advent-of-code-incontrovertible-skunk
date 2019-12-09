import org.scalatest.FlatSpec

class IntcodeComputerTest extends FlatSpec {
  "Calling process" should "get the examples for 2A correct" in {
    assertInputProducesOutput(List(1, 0, 0, 0, 99), List(2, 0, 0, 0, 99))
    assertInputProducesOutput(List(2, 3, 0, 3, 99), List(2, 3, 0, 6, 99))
    assertInputProducesOutput(List(2, 4, 4, 5, 99, 0), List(2, 4, 4, 5, 99, 9801))
    assertInputProducesOutput(List(1, 1, 1, 4, 99, 5, 6, 0, 99), List(30, 1, 1, 4, 2, 5, 6, 0, 99))
  }

  "Simple program from 5A" should "output the input value provided" in {
    val computer = new IntcodeComputer(List(3,0,4,0,99), List(27))
    computer.process()

    assert(computer.outputs.toList == List(27))
  }

  "First example with parameterModes" should "work" in {
    assertInputProducesOutput(List(1002,4,3,4,33), List(1002,4,3,4,99))
  }

  "Position mode example from 5B" should "output 0 for input 0" in {
    val computer = new IntcodeComputer(List(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), List(0))
    computer.process()

    assert(computer.outputs.toList == List(0))
  }

  "Position mode example from 5B" should "output 1 for non-zero input" in {
    val computer = new IntcodeComputer(List(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), List(12))
    computer.process()
    assert(computer.outputs.toList == List(1))

    val computer2 = new IntcodeComputer(List(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), List(-5))
    computer2.process()
    assert(computer2.outputs.toList == List(1))
  }

  "Immediate mode example from 5B" should "output 0 for input 0" in {
    val computer = new IntcodeComputer(List(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), List(0))
    computer.process()

    assert(computer.outputs.toList == List(0))
  }

  "Immediate mode example from 5B" should "output 1 for non-zero input" in {
    val computer = new IntcodeComputer(List(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), List(12))
    computer.process()
    assert(computer.outputs.toList == List(1))

    val computer2 = new IntcodeComputer(List(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), List(-5))
    computer2.process()
    assert(computer2.outputs.toList == List(1))
  }

  "Example 1 from 5B" should "output 1 for 8, 0 otherwise" in {
    val computer = new IntcodeComputer(List(3,9,8,9,10,9,4,9,99,-1,8), List(8))
    computer.process()
    assert(computer.outputs.toList == List(1))

    val computer2 = new IntcodeComputer(List(3,9,8,9,10,9,4,9,99,-1,8), List(7))
    computer2.process()
    assert(computer2.outputs.toList == List(0))
  }

  "Example 2 from 5B" should "output 1 for < 8, 0 otherwise" in {
    val computer = new IntcodeComputer(List(3,9,7,9,10,9,4,9,99,-1,8), List(7))
    computer.process()
    assert(computer.outputs.toList == List(1))

    val computer2 = new IntcodeComputer(List(3,9,7,9,10,9,4,9,99,-1,8), List(8))
    computer2.process()
    assert(computer2.outputs.toList == List(0))

    val computer3 = new IntcodeComputer(List(3,9,7,9,10,9,4,9,99,-1,8), List(9))
    computer3.process()
    assert(computer3.outputs.toList == List(0))
  }

  "Example 3 from 5B" should "output 1 for 8, 0 otherwise" in {
    val computer = new IntcodeComputer(List(3,3,1108,-1,8,3,4,3,99), List(8))
    computer.process()
    assert(computer.outputs.toList == List(1))

    val computer2 = new IntcodeComputer(List(3,3,1108,-1,8,3,4,3,99), List(7))
    computer2.process()
    assert(computer2.outputs.toList == List(0))

    val computer3 = new IntcodeComputer(List(3,3,1108,-1,8,3,4,3,99), List(9))
    computer3.process()
    assert(computer3.outputs.toList == List(0))
  }

  "Example 4 from 5B" should "output 1 for < 8, 0 otherwise" in {
    val computer = new IntcodeComputer(List(3,3,1107,-1,8,3,4,3,99), List(7))
    computer.process()
    assert(computer.outputs.toList == List(1))

    val computer2 = new IntcodeComputer(List(3,3,1107,-1,8,3,4,3,99), List(8))
    computer2.process()
    assert(computer2.outputs.toList == List(0))

    val computer3 = new IntcodeComputer(List(3,3,1107,-1,8,3,4,3,99), List(9))
    computer3.process()
    assert(computer3.outputs.toList == List(0))
  }

  "Larger example from 5B" should "output 999 if the input is below 8" in {
    val computer = new IntcodeComputer(List(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99), List(5))

    computer.process()

    assert(computer.outputs.toList == List(999))
  }

  "Larger example from 5B" should "output 1000 if the input is equal to 8" in {
    val computer = new IntcodeComputer(List(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99), List(8))

    computer.process()

    assert(computer.outputs.toList == List(1000))
  }

  "Larger example from 5B" should "output 1001 if the input is greater than 8" in {
    val computer = new IntcodeComputer(List(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99), List(100))

    computer.process()

    assert(computer.outputs.toList == List(1001))
  }

  "Example amplifier programs" should "get the right answers" in {
    assert(DaySevenHelpers.calculateMaxThrusterSignal(List(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)) == 43210)
    assert(DaySevenHelpers.calculateMaxThrusterSignal(List(3,23,3,24,1002,24,10,24,1002,23,-1,23,
      101,5,23,23,1,24,23,23,4,23,99,0,0)) == 54321)
    assert(DaySevenHelpers.calculateMaxThrusterSignal(List(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
      1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)) == 65210)
  }

  "Example amplifier programs with feedback" should "get the right answers" in {
    assert(DaySevenHelpers.calculateMaxThrusterSignalWithFeedbackLoop(List(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
      27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)) == 139629729)

    assert(DaySevenHelpers.calculateMaxThrusterSignalWithFeedbackLoop(List(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
      -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
      53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10)) == 18216)
  }

  "9A examples" should "produce example outputs" in {
    assertInputProducesOutput(List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99), List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99))
  }

  private def assertInputProducesOutput(input: List[Long], output: List[Int]): Unit = {
    assert(new IntcodeComputer(input).process() == output)
  }
}
