import org.scalatest.FlatSpec

class DayTwoTest extends FlatSpec {
  "Calculating fuel for mass" should "get the right example answers" in {
    assert(new DayTwo().processList(List(1,0,0,0,99)) == List(2,0,0,0,99))
    assert(new DayTwo().processList(List(2,3,0,3,99)) == List(2,3,0,6,99))
    assert(new DayTwo().processList(List(2,4,4,5,99,0)) == List(2,4,4,5,99,9801))
    assert(new DayTwo().processList(List(1,1,1,4,99,5,6,0,99)) == List(30,1,1,4,2,5,6,0,99))
  }
}
