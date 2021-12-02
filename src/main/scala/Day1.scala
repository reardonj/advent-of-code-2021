import cats.effect.IO
import fs2._

object Day1:
  def elements[F[_], I]: Pipe[F, I, Int] = stream =>
    stream.fold(0)((sum, _) => sum + 1)

  // https://adventofcode.com/2021/day/1
  def a[F[_]]: Pipe[F, String, Int] = stream =>
    stream
      .map(_.toInt)
      .sliding(2)
      .filter(chunk => chunk(1) > chunk(0))
      .through(elements)

  // https://adventofcode.com/2021/day/1#part2
  def b[F[_]]: Pipe[F, String, Int] = stream =>
    stream
      .map(_.toInt)
      .sliding(3)
      .map(_.toList.sum)
      .sliding(2)
      .filter(chunk => chunk(1) > chunk(0))
      .through(elements)
