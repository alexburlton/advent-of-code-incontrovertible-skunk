import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val lines = readFile("Test.txt")
    println(lines)
  }

  def readFile(filename: String): List[String] = {
    val src = Source.fromFile(filename)
    val list = src.getLines.toList
    src.close
    list
  }
}
