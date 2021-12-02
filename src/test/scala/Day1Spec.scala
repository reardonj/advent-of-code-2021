import org.scalatest.flatspec.AnyFlatSpec
import fs2.Stream
import org.scalatest.matchers.must.Matchers

class Day1Spec extends AnyFlatSpec with Matchers:
  val depthStream = Stream
    .emits(
      Seq(
        "199",
        "200",
        "208",
        "210",
        "200",
        "207",
        "240",
        "269",
        "260",
        "263"
      )
    )

  "part a" should "increase depth 7 times" in {
    depthStream.through(Day1.a).toList.head mustBe 7
  }

  "part b" should "increase depth 5 times" in {
    depthStream.through(Day1.b).toList.head mustBe 5
  }
