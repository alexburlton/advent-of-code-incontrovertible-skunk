
object Main {
  def days: List[AbstractPuzzle] = List(new DayTen())

  def main(args: Array[String]): Unit = {
    days.foreach { _.run() }
  }
}
