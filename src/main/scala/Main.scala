
object Main {
  def days: List[AbstractPuzzle] = List(new Day13())

  def main(args: Array[String]): Unit = {
    days.foreach { _.run() }
  }
}
