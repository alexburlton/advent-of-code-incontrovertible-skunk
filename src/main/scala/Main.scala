
object Main {
  def days: List[AbstractPuzzle] = List(new Day24())

  def main(args: Array[String]): Unit = {
    days.foreach(_.run())
  }
}
