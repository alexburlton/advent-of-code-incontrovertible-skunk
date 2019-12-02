import org.scalatest.FlatSpec

class AdventUtilsTest extends FlatSpec {
  "Read File" should "read the lines of the file into a list" in {
    val list = AdventUtils.readFile("Test.txt")
    assert(list == List("Line 1", "Line 2", "", "Line 4"))
  }
}
