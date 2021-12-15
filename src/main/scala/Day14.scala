import scala.annotation.tailrec
object Day14 extends Day:

  def a(stream: Iterator[String]): Any =
    val polymer = stream.next.toCharArray.toList
    // consume blank line
    stream.next
    val insertions = stream
      .map(_.split(" -> ") match {
        case Array(pair, i) => List(pair.toCharArray: _*) -> i(0)
      })
      .toMap

    val result =
      (1 to 10).foldLeft(polymer)((p, _) => polymerize(p, insertions))
    val elementsByFrequency =
      result.groupBy(identity).mapValues(_.length).toSeq.sortBy(_._2)
    elementsByFrequency.last._2 - elementsByFrequency.head._2

  def b(stream: Iterator[String]): Any =
    val polymer = stream.next.toCharArray.toSeq
    // consume blank line
    stream.next
    val insertions = stream
      .map(_.split(" -> ") match {
        case Array(pair, i) => (pair(0), pair(1)) -> i(0)
      })
      .toMap

    val (result, _) = polymerizeAndCount(40, polymer, insertions, Map(), Map())
    val elementsByFrequency =
      result.toSeq.sortBy(_._2)
    elementsByFrequency.last._2 - elementsByFrequency.head._2

  private def polymerize(
      polymer: List[Char],
      insertions: Map[List[Char], Char]
  ): List[Char] =
    polymer.head :: polymer
      .sliding(2, 1)
      .flatMap(pair => List(insertions(pair), pair(1)))
      .toList

  @tailrec
  private def polymerizeAndCount(
      steps: Int,
      pair: Seq[Char],
      insertions: Map[(Char, Char), Char],
      count: Map[Char, Long],
      solutions: Map[(Int, (Char, Char)), Map[Char, Long]]
  ): (Map[Char, Long], Map[(Int, (Char, Char)), Map[Char, Long]]) = pair match {
    case Seq(a, b, rest @ _*) =>
      val (leftCount, leftSolutions) =
        polymerizeAndCount(steps, (a, b), insertions, solutions)

      val total = mergeCounts(count, leftCount).updatedWith(a)(increment)
      polymerizeAndCount(
        steps,
        pair.tail,
        insertions,
        total,
        leftSolutions
      )
    case Seq(a) => (count.updatedWith(a)(increment), solutions)
    case _      => (count, solutions)
  }

  private def polymerizeAndCount(
      steps: Int,
      pair: (Char, Char),
      insertions: Map[(Char, Char), Char],
      solutions: Map[(Int, (Char, Char)), Map[Char, Long]]
  ): (Map[Char, Long], Map[(Int, (Char, Char)), Map[Char, Long]]) = pair match
    case (a, b) =>
      if steps == 0 then (Map(), solutions)
      else
        solutions
          .get((steps, pair))
          .map(solution => (solution, solutions))
          .getOrElse {
            val next = insertions(pair)
            val (leftCount, leftSolutions) =
              polymerizeAndCount(
                steps - 1,
                (a, next),
                insertions,
                solutions
              )
            val (rightCount, rightSolutions) = polymerizeAndCount(
              steps - 1,
              (next, b),
              insertions,
              leftSolutions
            )

            val total =
              mergeCounts(leftCount, rightCount).updatedWith(next)(increment)

            (total, rightSolutions + ((steps, pair) -> total))
          }

  private def increment(value: Option[Long]) = Some(1 + value.getOrElse(0L))

  private def mergeCounts(a: Map[Char, Long], b: Map[Char, Long]) =
    (a.keySet ++ b.keys).toSeq
      .map(l => l -> (a.getOrElse(l, 0L) + b.getOrElse(l, 0L)))
      .toMap
