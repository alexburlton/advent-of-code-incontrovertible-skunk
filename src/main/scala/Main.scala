
object Main {
  def days: List[AbstractPuzzle] = List(new Day10())

  def main(args: Array[String]): Unit = {
    days.foreach { _.run() }
  }
}
