
object Main {
  def days: List[AbstractPuzzle] = List(new DayOne(), new DayTwo())

  def main(args: Array[String]): Unit = {
    days.foreach { _.run() }
  }
}
