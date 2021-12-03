import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day3Spec extends AnyFlatSpec with Matchers:
  val stream = Seq(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  )

  "part a" should "yield 198" in {
    Day3.a(stream.iterator) mustBe 198
  }

  "part b" should "yield 230" in {
    Day3.b(stream.iterator) mustBe 230
  }
