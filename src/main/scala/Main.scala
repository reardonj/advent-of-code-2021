import cats.effect.{IO, IOApp}
import cats.effect.ExitCode
import fs2.{Stream, text}
import fs2.io.file.{Files, Path}
import fs2._
import java.nio.charset.Charset

object AdventOfCode extends IOApp:
  override def run(args: List[String]) = io
    .stdinUtf8[IO](4096)
    .through(text.lines)
    .dropLastIf(
      _.isEmpty
    ) // text.lines gets one last empty line, so discard it.
    .through(challenges(args.head))
    .map(_.toString)
    .through(io.stdoutLines())
    .compile
    .drain
    .map(_ => ExitCode.Success)

  val challenges: Map[String, Pipe[IO, String, Any]] = Map(
    "1a" -> Day1.a,
    "1b" -> Day1.b,
    "2a" -> Day2.a,
    "2b" -> Day2.b
  )
