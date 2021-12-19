package com.jmreardon.aoc2021

import scala.annotation.tailrec
object Day3 extends Day:
  def a(stream: Iterator[String]): Any = {
    val lines :: sums = stream
      .map(line => 1 +: line.split("").map(_.toInt).toSeq)
      .foldLeft(Seq[Int]())((sum, next) =>
        sum.zipAll(next, 0, 0).map { case (a, b) => a + b }
      )
    val gamma = binary(sums.map(n => if n >= lines / 2 then 1 else 0))
    val epsilon = binary(sums.map(n => if n <= lines / 2 then 1 else 0))

    gamma * epsilon
  }

  def b(stream: Iterator[String]): Any = {
    val lines = stream.toSeq
    val oxygen =
      filterRatings(lines, 0, Ordering.by { case (k, v) => (v.length, k) })
    val scrubber =
      filterRatings(lines, 0, Ordering.by { case (k, v) => (-v.length, -k) })

    oxygen * scrubber
  }

  @tailrec
  def filterRatings(
      lines: Seq[String],
      index: Int,
      selector: Ordering[(Char, Seq[?])]
  ): Int = {
    val (_, selected) = lines.groupBy(_.apply(index)).max(selector)

    if (selected.length == 1) then Integer.parseInt(selected.head, 2)
    else filterRatings(selected, index + 1, selector)
  }

  def binary(seq: Seq[Int]): Int = seq.reverse.zipWithIndex.map {
    case (value, pos) =>
      value * Math.pow(2, pos).toInt
  }.sum
