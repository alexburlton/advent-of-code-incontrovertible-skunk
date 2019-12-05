import org.scalatest.FlatSpec

class IntcodeComputerTest extends FlatSpec {
  "Calling process" should "get the examples for 2A correct" in {
    assertInputProducesOutput(List(1, 0, 0, 0, 99), List(2, 0, 0, 0, 99))
    assertInputProducesOutput(List(2, 3, 0, 3, 99), List(2, 3, 0, 6, 99))
    assertInputProducesOutput(List(2, 4, 4, 5, 99, 0), List(2, 4, 4, 5, 99, 9801))
    assertInputProducesOutput(List(1, 1, 1, 4, 99, 5, 6, 0, 99), List(30, 1, 1, 4, 2, 5, 6, 0, 99))
  }

  "Simple program from 5A" should "output the input value provided" in {
    val computer = new IntcodeComputer(List(3,0,4,0,99), 27)
    computer.process()

    assert(computer.outputs.toList == List(27))
  }

  "First example with parameterModes" should "work" in {
    assertInputProducesOutput(List(1002,4,3,4,33), List(1002,4,3,4,99))
  }

  private def assertInputProducesOutput(input: List[Int], output: List[Int]): Unit = {
    assert(new IntcodeComputer(input).process() == output)
  }
}
