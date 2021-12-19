package com.jmreardon.aoc2021

import scala.annotation.tailrec
object Day5 extends Day:
  type Point = (Int, Int)
  type Segment = (Point, Point)

  def a(stream: Iterator[String]): Any =
    stream
      .map(lineToSegment)
      .flatten
      .filter(isStraight)
      .flatMap(segmentToPoints)
      .toSeq
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .filter { case _ -> count => count > 1 }
      .size

  def b(stream: Iterator[String]): Any =
    stream
      .map(lineToSegment)
      .flatten
      .flatMap(segmentToPoints)
      .toSeq
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .filter { case _ -> count => count > 1 }
      .size

  private def isStraight(segment: Segment) = segment match {
    case (x1, y1) -> (x2, y2) => x1 == x2 || y1 == y2
  }

  private def segmentToPoints(segment: Segment): Seq[Point] = {
    @tailrec
    def segmentToPoints(segment: Segment, acc: Seq[Point]): Seq[Point] =
      segment match {
        case (x1, y1) -> (x2, y2) =>
          if x1 == x2 && y1 == y2 then acc
          else
            val nextPoint = (nextNum(x1, x2), nextNum(y1, y2))
            segmentToPoints(nextPoint -> (x2, y2), acc :+ (x1, y1))
      }

    def nextNum(a: Int, b: Int) =
      (if a < b then a + 1 else if a > b then a - 1 else a)

    segmentToPoints(segment, Seq(segment._2))
  }

  // Pattern for: x1,y1 -> x2,y2
  private val linePattern = """(\d*),(\d*)\s*->\s*(\d*),(\d*)""".r

  private def lineToSegment(line: String): Option[Segment] = line match {
    case linePattern(x1, y1, x2, y2) =>
      Some((x1.toInt, y1.toInt) -> (x2.toInt, y2.toInt))
    case _ => None
  }
