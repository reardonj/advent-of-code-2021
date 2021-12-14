object Day13 extends Day:

  def a(stream: Iterator[String]): Any = {
    val (dots, folds) = parseStream(stream)
    dots.map(_.fold(folds.head)).toSet.size
  }

  def b(stream: Iterator[String]): Any = {
    val (dots, folds) = parseStream(stream)
    val foldedDots =
      folds.foldLeft(dots.toSet)((dots, fold) => dots.map(_.fold(fold)))

    dotsToString(foldedDots)
  }

  case class Point(x: Int, y: Int):
    def fold(foldAt: Point): Point =
      Point(
        if foldAt.x == 0 || x < foldAt.x then x
        else -1 * (x - foldAt.x) + foldAt.x,
        if foldAt.y == 0 || y < foldAt.y then y
        else -1 * (y - foldAt.y) + foldAt.y
      )

  private def dotsToString(dots: Set[Point]): String =
    val maxX = dots.maxBy(_.x).x
    val maxY = dots.maxBy(_.y).y

    val lines = for {
      y <- 0 to maxY
    } yield {
      "\n" + String.join(
        "",
        (0 to maxX).map(x =>
          if dots.contains(Point(x, y)) then "#" else " "
        ): _*
      )
    }

    String.join("", lines: _*)

  private def parseStream(
      stream: Iterator[String]
  ): (Seq[Point], Seq[Point]) = {
    val points = stream
      .takeWhile(!_.isBlank)
      .map(_.split(",") match { case Array(x, y) => Point(x.toInt, y.toInt) })
      .toSeq

    val folds = stream
      .map(_.split("=") match {
        case Array("fold along y", y) => Point(0, y.toInt)
        case Array("fold along x", x) => Point(x.toInt, 0)
      })
      .toSeq

    (points, folds)
  }
