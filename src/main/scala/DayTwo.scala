import util.control.Breaks._

import scala.collection.mutable

class DayTwo extends AbstractPuzzle(2) {

  val inputCommands: List[Int] = inputLines.head.split(",").map(s => s.toInt).toList

  override def partA(): Int = {
    val substituted = makeInitialSubstitution(inputCommands, 12, 2)
    processList(substituted).head
  }

  override def partB(): Int = findNounAndVerbForOutput(inputCommands, 19690720)

  def findNounAndVerbForOutput(commands: List[Int], output: Int): Int = {
    for (noun <- 0 to 99) {
      for (verb <- 0 to 99) {
        val substituted = makeInitialSubstitution(commands, noun, verb)
        val result = processList(substituted).head
        if (result == output) {
          return 100 * noun + verb
        }
      }
    }

    -1
  }

  def makeInitialSubstitution(commands: List[Int], noun: Int, verb: Int): List[Int] = {
    val mutableList = new mutable.ListBuffer[Int]()
    mutableList.addAll(commands)

    mutableList(1) = noun
    mutableList(2) = verb

    mutableList.toList
  }

  def processList(commands: List[Int]): List[Int] = {
    val memory = new mutable.ListBuffer[Int]()
    memory.addAll(commands)

    var instructionPointer = 0
    breakable {
      while (true) {
        val instruction = memory(instructionPointer)
        if (instruction == 99) break

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

    memory.toList
  }
}
