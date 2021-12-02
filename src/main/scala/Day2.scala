import fs2._

object Day2:
  // https://adventofcode.com/2021/day/2
  def a[F[_]]: Pipe[F, String, Int] = stream =>
    stream
      .map(directionToVector)
      .fold((0, 0)) { case ((ax, ay), (bx, by)) => (ax + bx, ay + by) }
      .map { case (x, y) => x * y }

  // https://adventofcode.com/2021/day/2#part2
  def b[F[_]]: Pipe[F, String, Int] = stream =>
    stream
      .map(_.toInt)

  private def directionToVector(line: String): (Int, Int) =
    line.split(" ") match {
      case Array("forward", x) => (x.toInt, 0)
      case Array("down", x)    => (0, x.toInt)
      case Array("up", x)      => (0, -x.toInt)
    }
