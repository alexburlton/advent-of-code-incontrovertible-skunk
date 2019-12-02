import scala.io.Source

object AdventUtils {
  def readFile(filename: String): List[String] = {
    val src = Source.fromFile(filename)
    val list = src.getLines.toList
    src.close
    list
  }
}

