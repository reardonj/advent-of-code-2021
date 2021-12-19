package com.jmreardon.aoc2021

object Day7 extends Day:
  def a(stream: Iterator[String]): Any = {
    def distancesTo(value: Int, list: Seq[Int]) =
      list.map(x => Math.abs(x - value)).sum

    val sorted =
      stream.flatMap(_.split(",")).map(_.toInt).toSeq.sortBy(identity)

    // Get both medians.
    sorted.drop(sorted.length / 2 - 1).take(2).map(distancesTo(_, sorted)).min
  }

  def b(stream: Iterator[String]): Any = {
    def distancesTo(value: Int, list: Seq[Int]) =
      // Fuel cost is triangle number sequence
      list.map(x => Math.abs(x - value)).map(n => n * (n + 1) / 2).sum

    val positions = stream.flatMap(_.split(",")).map(_.toInt).toSeq
    val averagePosition = positions.sum.toDouble / positions.length
    Seq(
      distancesTo(averagePosition.floor.toInt, positions),
      distancesTo(averagePosition.ceil.toInt, positions)
    ).min
  }
