import java.nio.charset.Charset
import scala.io.BufferedSource
import scala.io.Source
import scala.util.Try
import java.io.FileNotFoundException
import java.time.Instant
import java.time.Duration

object AdventOfCode:
  @main def run(challengesToRun: String*) = for {
    challenge <- challengesToRun
    (challengeFileName, pipeline) = challenges(challenge)
    (name, file, solve) <- Seq(
      (s"$challenge -> real", challengeFileName, pipeline),
      (s"$challenge -> test", challengeFileName + ".example", pipeline)
    )
    stream <- Try(Source.fromResource(file)).toOption
  } yield {
    val lines = stream.getLines.toSeq.iterator

    val start = System.nanoTime();
    val solution = solve(lines).toString
    val ends = System.nanoTime();

    println(s"$name: $solution")
    println(s"      ${Duration.ofNanos(ends - start).toMillis} ms")
  }

  val days = Seq(
    Day1,
    Day2,
    Day3,
    Day4,
    Day5,
    Day6,
    Day7,
    Day8,
    Day9,
    Day10,
    Day11,
    Day12,
    Day13,
    Day14,
    Day15,
    Day16
  )

  val challenges: Map[String, (String, Iterator[String] => Any)] =
    days.zipWithIndex.flatMap { case (day, idx) =>
      day.challenges.map { case (name, challenge) =>
        val day = idx + 1
        s"${day}$name" -> (s"day${day}.txt", challenge)
      }
    }.toMap
