import scala.annotation.tailrec
object Day17 extends Day:
  def a(stream: Iterator[String]): Any =
    val (xMin, xMax, yMin, yMax) = parse(stream)
    Scenario(xMin, xMax, yMin, yMax).maximizeY()

  def b(stream: Iterator[String]): Any =
    val (xMin, xMax, yMin, yMax) = parse(stream)
    Scenario(xMin, xMax, yMin, yMax).searchHits.size

  private def parse(stream: Iterator[String]) =
    val parser = """target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)""".r
    stream.next match {
      case parser(xMin, xMax, yMin, yMax) =>
        (xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt)
    }

  // Brute forced solutions just scanning valid ranges.
  private class Scenario(xMin: Int, xMax: Int, yMin: Int, yMax: Int):

    def maximizeY() =
      val minInitialX = findMinInitialX(xMin).get
      val maxInitialX = xMax
      searchMaximumY(minInitialX, maxInitialX)

    def searchHits =
      val minInitialX = findMinInitialX(xMin).get
      val maxInitialX = xMax
      val intersections = for {
        y <- (yMin to 200).toSet
        x <- minInitialX to maxInitialX
        if intersects(x, y).nonEmpty
      } yield (x, y)
      intersections.toSet

    @tailrec
    private def searchMaximumY(
        minXVelocity: Int,
        maxXVelocity: Int,
        maxHeight: Option[Int] = None,
        yVelocity: Int = 1
    ): Int =
      val maxCandidate =
        (minXVelocity to maxXVelocity)
          .flatMap(x => intersects(x, yVelocity))
          .maxOption
      val nextMax = (maxHeight ++ maxCandidate).maxOption
      if (yVelocity > 200) then nextMax.getOrElse(0)
      else searchMaximumY(minXVelocity, maxXVelocity, nextMax, yVelocity + 1)

    @tailrec
    private def intersects(
        xVelocity: Int,
        yVelocity: Int,
        xPos: Int = 0,
        yPos: Int = 0,
        heights: List[Int] = Nil
    ): List[Int] =
      if xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax then
        heights
      else if yPos <= yMin then Nil
      else
        intersects(
          Math.max(0, xVelocity - 1),
          yVelocity - 1,
          xPos + xVelocity,
          yPos + yVelocity,
          yPos :: heights
        )

    private def findMinInitialX(xMin: Int) = (1 to Int.MaxValue).find { x =>
      val maxX = x * (x + 1) / 2
      maxX > xMin
    }
