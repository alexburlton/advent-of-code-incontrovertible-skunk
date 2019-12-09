
object Main {
  def days: List[AbstractPuzzle] = List(new DayNine())

  def main(args: Array[String]): Unit = {
    days.foreach { _.run() }
  }
}
