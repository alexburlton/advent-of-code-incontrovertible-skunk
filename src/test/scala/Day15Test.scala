import java.awt.Point

import org.scalatest.FlatSpec

import scala.collection.mutable

class Day15Test extends FlatSpec {
  "Generating the full map" should "match the output file and provide the correct path length and oxygen time" in {
    val inputLines = AdventUtils.readFile("inputs/Day15")
    val intcode: List[Long] = AdventUtils.parseAsIntcodeInput(inputLines)

    val map = new mutable.HashMap[Point, String]
    Day15Helpers.fullyExploreArea(intcode, map)

    val mapFromFile = readMapFile("testInputs/Day15MapWithDrone")

    assert(AdventUtils.toCoordinateList(map.toMap, " ") == AdventUtils.toCoordinateList(mapFromFile, " "))
    assert(Day15Helpers.findShortestPathToOxygen(map) == 236)
    assert(Day15Helpers.fillWithOxygen(map) == 368)
  }

  private def readMapFile(filename: String): Map[Point, String] = {
    val mapLines = AdventUtils.readFile(filename)
    val hmPointToType = new mutable.HashMap[Point, String]

    for (y <- mapLines.indices) {
      val line = mapLines(y)
      for (x <- line.indices) {
        val char = line.charAt(x)

        hmPointToType.put(new Point(x, y), char.toString)
      }
    }

    hmPointToType.toMap
  }
}
