import scala.annotation.tailrec
object Day3 extends Day:
  def a(stream: Iterator[String]): Any = {
    val lines :: sums = stream
      .map(line => 1 +: line.split("").map(_.toInt).toSeq)
      .foldLeft(Seq[Int]())((sum, next) =>
        sum.zipAll(next, 0, 0).map { case (a, b) => a + b }
      )
    val averages = sums.map(_ * 1d / lines).reverse
    val gamma = binary(averages.map(n => if n >= 0.5 then 1 else 0))
    val epsilon = binary(averages.map(n => if n <= 0.5 then 1 else 0))

    gamma * epsilon
  }
  def b(stream: Iterator[String]): Any = {
    val lines = stream.map(line => line.split("").map(_.toInt).toSeq).toSeq
    val oxygen =
      filterRatings(lines, 0, Ordering.by { case (k, v) => (v.length, k) })
    val scrubber =
      filterRatings(lines, 0, Ordering.by { case (k, v) => (-v.length, -k) })

    oxygen * scrubber
  }

  @tailrec
  def filterRatings(
      lines: Seq[Seq[Int]],
      index: Int,
      selector: Ordering[(Int, Seq[?])]
  ): Int = {
    val (_, selected) = lines.groupBy(_.apply(index)).max(selector)

    if (selected.length == 1) then binary(selected.head.reverse)
    else filterRatings(selected, index + 1, selector)
  }

  def binary(seq: Seq[Int]): Int = seq.zipWithIndex.map { case (value, pos) =>
    value * Math.pow(2, pos).toInt
  }.sum
