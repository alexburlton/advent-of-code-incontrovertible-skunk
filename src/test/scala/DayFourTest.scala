import org.scalatest.FlatSpec

class DayFourTest extends FlatSpec {
  "Has repeated digits" should "work" in {
    assert(!DayFourHelpers.hasRepeatedDigits(123456))
    assert(DayFourHelpers.hasRepeatedDigits(111223))
    assert(DayFourHelpers.hasRepeatedDigits(111233))
    assert(DayFourHelpers.hasRepeatedDigits(111222))
    assert(DayFourHelpers.hasRepeatedDigits(112223))
    assert(DayFourHelpers.hasRepeatedDigits(111123))
  }

  "Has exact pair" should "work" in {
    assert(!DayFourHelpers.hasExactPair(123456))
    assert(DayFourHelpers.hasExactPair(111223))
    assert(DayFourHelpers.hasExactPair(111233))
    assert(!DayFourHelpers.hasExactPair(111222))
    assert(DayFourHelpers.hasExactPair(112223))
    assert(!DayFourHelpers.hasExactPair(111123))
  }

  "Digits are not decreasing" should "work" in {
    assert(DayFourHelpers.digitsAreNotDecreasing(111111))
    assert(DayFourHelpers.digitsAreNotDecreasing(113447))
    assert(!DayFourHelpers.digitsAreNotDecreasing(123434))
    assert(!DayFourHelpers.digitsAreNotDecreasing(123451))
  }

  "Valid password A" should "get the right example answers" in {
    assert(DayFourHelpers.validPasswordPartA(111111))
    assert(!DayFourHelpers.validPasswordPartA(223450))
    assert(!DayFourHelpers.validPasswordPartA(123789))
  }

  "Valid password B" should "get the right example answers" in {
    assert(DayFourHelpers.validPasswordPartB(112233))
    assert(!DayFourHelpers.validPasswordPartB(123444))
    assert(DayFourHelpers.validPasswordPartB(111122))
  }
}
