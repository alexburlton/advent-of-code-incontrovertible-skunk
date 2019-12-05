
object Main {
  def days: List[AbstractPuzzle] = List(new DayOne(), new DayTwo(), new DayThree(), new DayFour(), new DayFive())

  def main(args: Array[String]): Unit = {
    days.foreach { _.run() }
  }
}
