import java.nio.charset.Charset
import scala.io.BufferedSource
import scala.io.Source
import scala.util.Try

object AdventOfCode:
  @main def run(challengesToRun: String*) = for {
    challenge <- challengesToRun
    (challengeFileName, pipeline) = challenges(challenge)
    (name, file, solve) <- Seq(
      (challenge + " example", challengeFileName + ".example", pipeline),
      (challenge, challengeFileName, pipeline)
    )
  } yield {
    Try(println(name + ": " + solve(Source.fromResource(file).getLines)))
  }

  val challenges: Map[String, (String, Iterator[String] => Any)] =
    Seq(Day1, Day2, Day3, Day4, Day5).zipWithIndex.flatMap { case (day, idx) =>
      day.challenges.map { case (name, challenge) =>
        val day = idx + 1
        s"${day}$name" -> (s"day${day}.txt", challenge)
      }
    }.toMap
