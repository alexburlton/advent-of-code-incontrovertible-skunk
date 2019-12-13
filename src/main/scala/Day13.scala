import java.awt.Point

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

class Day13 extends AbstractPuzzle(13) {
  val intcode = AdventUtils.parseAsIntcodeInput(inputLines)

  override def partA(): Any = {
    val computer = new IntcodeComputer(intcode)
    computer.process()

    var count = 0
    val outputs = computer.outputs
    for (i <- outputs.indices) {
      if ((i + 1) % 3 == 0 && outputs(i) == 2) {
        count += 1
      }
    }
    count
  }

  override def partB(): Any = {
    val computer = new IntcodeComputer(intcode)
    computer.substituteValue(0, 2)

    val futureComputer = new IntcodeComputer(intcode)
    computer.substituteValue(0, 2)

    computer.process()
    futureComputer.process()

    val breakoutState = new BreakoutState(computer)
    breakoutState.initialise()

    while (!breakoutState.finished()) {
      val breakoutStateTrial = breakoutState.copy()
      breakoutStateTrial.doTick(0)
      val paddleMove = breakoutStateTrial.getProposedPaddleMovement

      //Iterate our actual state
      breakoutState.doTick(paddleMove, logging = true)
    }
  }

  class BreakoutState(val computer: IntcodeComputer) {
    val tileStates: mutable.HashMap[Point, TileState] = mutable.HashMap[Point, TileState]()
    var score: Long = 0

    var previousBall: Point = _

    def initialise(): Unit = {
      computer.process()
      val outputs = computer.outputs
      val groupedOutputs = outputs.grouped(3).toList

      val tiles = groupedOutputs.filter(_(0) > -1).map { it => TileState(new Point(it(0).toInt, it(1).toInt), it(2)) }
      tiles.foreach { tile =>
        tileStates.put(tile.point, tile)
      }
    }

    def copy(): BreakoutState = {
      val newComputer = computer.copy()

      val newState = new BreakoutState(newComputer)
      newState.score = this.score
      newState.tileStates.addAll(this.tileStates)
      newState
    }

    def doTick(paddleMovement: Int, logging: Boolean = false): Unit = {
      previousBall = getBallPosition

      computer.clearOutputs()
      computer.processWithInput(paddleMovement)

      val outputs = computer.outputs.toList
      val groupedOutputs = outputs.grouped(3).toList

      val tiles = groupedOutputs.filter(_(0) > -1).map { it => TileState(new Point(it(0).toInt, it(1).toInt), it(2)) }
      tiles.foreach { tile =>
        tileStates.put(tile.point, tile)
      }

      updateScore(groupedOutputs, logging)
    }

    def finished(): Boolean = {
      if (getBlockCount == 0) {
        println(s"ALL BLOCKS DONEL: $score")
        true
      } else if (getBallPosition.y == getPaddlePosition.y) {
        println(s"DIED: Ball [$getBallPosition], Paddle [$getPaddlePosition]")
        true
      } else {
        false
      }
    }

    private def getBlockCount = tileStates.values.count(_.tileType == 2)

    private def getBallPosition: Point = {
      tileStates.values.filter(_.tileType == 4).head.point
    }

    private def getPaddlePosition: Point = {
      tileStates.values.filter(_.tileType == 3).head.point
    }

    def getProposedPaddleMovement: Int = {
      val ball = getBallPosition
      val paddle = getPaddlePosition

      if (ball.y < previousBall.y && previousBall.y == paddle.y - 1) {
        //we're already safe, introduce some randomness so we actually finish
        println(s"Doing random move. Ball: $ball, previous: $previousBall")
        val moves = List(0, ball.x - previousBall.x compare 0)
        moves(Random.nextInt(2))
      } else {
        ball.x - paddle.x compare 0
      }
    }

    private def updateScore(groupedOutputs: List[List[Long]], logging: Boolean = false): Unit = {
      val scoringBits = groupedOutputs.filter(_(0) == -1)
      if (scoringBits.nonEmpty) {
        score = scoringBits.head(2)
        if (logging) {
          println(s"Block hit! Blocks remaining: $getBlockCount, Score: $score")
        }
      }
    }

    private def printGrid(): Unit = {
      val allPoints = tileStates.keys
      val xRange = allPoints.minBy(_.x).x to allPoints.maxBy(_.x).x
      val yRange = allPoints.minBy(_.y).y to allPoints.maxBy(_.y).y

      for (y <- yRange) {
        val list = ListBuffer[String]()
        for (x <- xRange) {
          val pt = new Point(x, y)
          val state = tileStates(pt)

          list.addOne(state.getStringRepresentation)
        }

        val str = list.mkString
        println(str)
      }

      println(s"Score: $score")
    }
  }

  case class TileState(point: Point, tileType: Long) {
    def getStringRepresentation: String = {
      tileType match {
        case 0 => " "
        case 1 => "@"
        case 2 => "#"
        case 3 => "-"
        case 4 => "o"
      }
    }
  }
}
