import java.nio.charset.Charset
import scala.io.BufferedSource
import scala.io.Source
import scala.util.Try
import java.io.FileNotFoundException

object AdventOfCode:
  @main def run(challengesToRun: String*) = for {
    challenge <- challengesToRun
    (challengeFileName, pipeline) = challenges(challenge)
    (name, file, solve) <- Seq(
      (s"$challenge -> test", challengeFileName + ".example", pipeline),
      (s"$challenge -> real", challengeFileName, pipeline)
    )
    stream <- Try(Source.fromResource(file)).toOption
  } yield {
    println(name + ": " + solve(stream.getLines))
  }

  val challenges: Map[String, (String, Iterator[String] => Any)] =
    Seq(
      Day1,
      Day2,
      Day3,
      Day4,
      Day5,
      Day6,
      Day7,
      Day8,
      Day9
    ).zipWithIndex.flatMap { case (day, idx) =>
      day.challenges.map { case (name, challenge) =>
        val day = idx + 1
        s"${day}$name" -> (s"day${day}.txt", challenge)
      }
    }.toMap
