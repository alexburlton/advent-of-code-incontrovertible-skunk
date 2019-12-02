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
    val mutableList = new mutable.ListBuffer[Int]()
    mutableList.addAll(commands)

    var instructionPointer = 0
    breakable {
      while (true) {
        val command = mutableList(instructionPointer)
        if (command == 99) break

        val posOne = mutableList(instructionPointer + 1)
        val posTwo = mutableList(instructionPointer + 2)
        val posToWrite = mutableList(instructionPointer + 3)

        if (command == 1) {
          mutableList(posToWrite) = mutableList(posOne) + mutableList(posTwo)
        } else {
          mutableList(posToWrite) = mutableList(posOne) * mutableList(posTwo)
        }

        instructionPointer += 4
      }
    }

    mutableList.toList
  }
}
