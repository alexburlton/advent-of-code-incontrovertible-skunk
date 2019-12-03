import java.awt.Point

import org.scalatest.FlatSpec

class DayThreeTest extends FlatSpec {
  "Constructing a wire" should "produce the correct path" in {
    val wire = new Wire("R8,U5,L5,D3")

    assert(wire.coords.size == 21)
    assert(wire.coords.last == new Point(3, 2))
  }

  "Finding intersections" should "work as in the example" in {
    val wireA = new Wire("R8,U5,L5,D3")
    val wireB = new Wire("U7,R6,D4,L4")

    val crossings = WireHelpers.findCrossings(wireA, wireB)
    assert(crossings == Vector(new Point(6, 5), new Point(3, 3)))
  }

  "Manhatten distance" should "be correct" in {
    assert(WireHelpers.getManhattenDistance(new Point(-3, 5)) == 8)
    assert(WireHelpers.getManhattenDistance(new Point(11, -100)) == 111)
    assert(WireHelpers.getManhattenDistance(new Point(0, 3)) == 3)
  }

  "Part 1A" should "get the right example answers" in {
    assert(WireHelpers.getMinCrossingManhattenDistance(new Wire("R8,U5,L5,D3"), new Wire("U7,R6,D4,L4")) == 6)
    assert(WireHelpers.getMinCrossingManhattenDistance(new Wire("R75,D30,R83,U83,L12,D49,R71,U7,L72"), new Wire("U62,R66,U55,R34,D71,R55,D58,R83")) == 159)
    assert(WireHelpers.getMinCrossingManhattenDistance(new Wire("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"), new Wire("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")) == 135)
  }

  "Part 1B" should "get the right example answers" in {
    assert(WireHelpers.getMinSignalDelay(new Wire("R8,U5,L5,D3"), new Wire("U7,R6,D4,L4")) == 30)
    assert(WireHelpers.getMinSignalDelay(new Wire("R75,D30,R83,U83,L12,D49,R71,U7,L72"), new Wire("U62,R66,U55,R34,D71,R55,D58,R83")) == 610)
    assert(WireHelpers.getMinSignalDelay(new Wire("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"), new Wire("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")) == 410)
  }
}
