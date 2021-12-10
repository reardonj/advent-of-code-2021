import scala.collection.SeqLike
import scala.annotation.tailrec
object Day9 extends Day:
  case class Point(x: Int, y: Int)

  def a(stream: Iterator[String]): Any = {
    val map = readMap(stream)
    val lookup = toLookup(map)

    val lowPoints = for {
      y <- 0 until map.length
      x <- 0 until map(0).length
      height = map(y)(x)
      neighbours = neighbourPoints(Point(x, y)).map(lookup).flatten
      if neighbours.forall(height < _)
    } yield {
      height
    }

    lowPoints.map(_ + 1).sum
  }

  def b(stream: Iterator[String]): Any = {

    val map = readMap(stream)
    val lookup = toLookup(map)

    val open = (for {
      y <- 0 until map.length
      x <- 0 until map(0).length
    } yield {
      Point(x, y)
    }).toSet

    @tailrec
    def findBasins(
        open: Set[Point],
        basins: Seq[Set[Point]] = Seq()
    ): Seq[Set[Point]] = {
      if open.isEmpty then basins
      else
        val (rest, basin) =
          buildBasin(Set(open.head), open - open.head, Set[Point]())
        findBasins(rest, basins :+ basin)
    }

    @tailrec
    def buildBasin(
        frontier: Set[Point],
        open: Set[Point],
        basin: Set[Point]
    ): (Set[Point], Set[Point]) =
      if frontier.isEmpty then (open, basin)
      else {
        val next = frontier.head
        lookup(next) match {
          case None    => buildBasin(frontier - next, open, basin)
          case Some(9) => buildBasin(frontier - next, open, basin)
          case Some(_) =>
            val neighbours = neighbourPoints(next).toSet.intersect(open)
            buildBasin(
              frontier - next ++ neighbours,
              open -- neighbours,
              basin + next
            )
        }
      }

    findBasins(open)
      .map(_.size)
      .sorted
      .reverse
      .take(3)
      .fold(1)(_ * _)
  }

  private def neighbourPoints(point: Point) = point match {
    case Point(x, y) =>
      Seq(
        Point(x - 1, y),
        Point(x + 1, y),
        Point(x, y + 1),
        Point(x, y - 1)
      )
  }

  private def readMap(stream: Iterator[String]) =
    stream.map(_.map(_.intValue - '0').toArray).toArray

  private def toLookup(array: Array[Array[Int]]) = {
    val lookup = array.map(_.lift).lift
    (point: Point) => lookup(point.y).map(_.apply(point.x)).flatten
  }
