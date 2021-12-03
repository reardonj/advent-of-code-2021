import java.nio.charset.Charset
import scala.io.BufferedSource

object AdventOfCode:
  @main def run(challenge: String) = println(
    challenges(challenge)(BufferedSource(System.in).getLines)
  )

  val challenges: Map[String, Iterator[String] => Any] =
    Seq(Day1, Day2).zipWithIndex.flatMap { case (day, idx) =>
      day.challenges.map { case (name, challenge) =>
        s"${idx + 1}$name" -> challenge
      }
    }.toMap
