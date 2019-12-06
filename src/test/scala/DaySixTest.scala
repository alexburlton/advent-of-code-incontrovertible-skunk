import org.scalatest.FlatSpec

class DaySixTest extends FlatSpec {
  "6A example" should "be correct" in {
    val lines = AdventUtils.readFile("Day6AExample")
    val dag = DaySixHelpers.constructDag(lines)

    assert(dag.getAllChildren("D").size == 3)
    assert(dag.getAllChildren("L").size == 7)
    assert(dag.getAllChildren("COM").isEmpty)
    assert(dag.sumChildCountForAllNodes() == 42)
  }

  "6B example" should "be correct" in {
    val lines = AdventUtils.readFile("Day6BExample")
    val dag = DaySixHelpers.constructDag(lines)

    assert(dag.getShortestPathBetween("YOU", "SAN") == 4)
  }
}
