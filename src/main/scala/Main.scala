
object Main {
  def days: List[AbstractPuzzle] = List(new DayOne(), new DayTwo(), new DayThree())

  def main(args: Array[String]): Unit = {
    days.foreach { _.run() }
  }
}
