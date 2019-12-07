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

  private def assertInputProducesOutput(input: List[Int], output: List[Int]): Unit = {
    assert(new IntcodeComputer(input).process() == output)
  }
}
