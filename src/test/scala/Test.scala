import org.scalatest.FlatSpec

class Test extends FlatSpec {
  "Read File" should "read the lines of the file into a list" in {
    val list = Main.readFile("Test.txt")
    assert(list == List("Line 1", "Line 2"))
  }
}
