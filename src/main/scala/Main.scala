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
    Seq(Day1, Day2).zipWithIndex.flatMap { case (day, idx) =>
      day.challenges.map { case (name, challenge) =>
        s"${idx + 1}$name" -> (s"day${idx + 1}.txt", challenge)
      }
    }.toMap
