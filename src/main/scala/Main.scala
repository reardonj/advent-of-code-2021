import java.nio.charset.Charset
import scala.io.BufferedSource
import scala.io.Source

object AdventOfCode:
  @main def run(challengesToRun: String*) = for {
    challenge <- challengesToRun
  } yield {
    val (file, pipeline) = challenges(challenge)
    println(challenge + ": " + pipeline(Source.fromResource(file).getLines))
  }

  val challenges: Map[String, (String, Iterator[String] => Any)] =
    Seq(Day1, Day2, Day3, Day4).zipWithIndex.flatMap { case (day, idx) =>
      day.challenges.map { case (name, challenge) =>
        val day = idx + 1
        s"${day}$name" -> (s"day${day}.txt", challenge)
      }
    }.toMap
