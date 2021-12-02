import org.scalatest.flatspec.AnyFlatSpec
import fs2.Stream
import org.scalatest.matchers.must.Matchers

class Day2Spec extends AnyFlatSpec with Matchers:
  val stream = Stream
    .emits(
      Seq(
        "forward 5",
        "down 5",
        "forward 8",
        "up 3",
        "down 8",
        "forward 2"
      )
    )

  "part a" should "yield 150" in {
    stream.through(Day2.a).toList.head mustBe 150
  }

  "part a" should "yield 900" in {
    stream.through(Day2.b).toList.head mustBe 900
  }
