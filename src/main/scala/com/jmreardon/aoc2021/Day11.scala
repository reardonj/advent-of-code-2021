package com.jmreardon.aoc2021

import scala.annotation.tailrec
object Day11 extends Day:
  def a(stream: Iterator[String]): Any =
    takeSteps(loadMap(stream), 100)

  def b(stream: Iterator[String]): Any =
    stepUntilAllFlash(loadMap(stream), 1)

  @tailrec
  private def stepUntilAllFlash(map: Array[Int], steps: Int): Int =
    if step(map) == 100 then steps else stepUntilAllFlash(map, steps + 1)

  private def takeSteps(map: Array[Int], steps: Int) = {
    val flashes = Seq.tabulate(steps)(_ => step(map)).sum
    //println(printMap(map))

    flashes
  }

  private def step(map: Array[Int]): Int = {
    val toProcess = scala.collection.mutable.Queue((0 until 100): _*)
    while (toProcess.nonEmpty) {
      val mapPosition = toProcess.dequeue
      map(mapPosition) = 1 + map(mapPosition)
      if map(mapPosition) == 10 then
        toProcess.enqueueAll(neighbours(mapPosition))
    }

    map.mapInPlace(e => if e >= 10 then 0 else e)
    map.count(_ == 0)
  }

  private def neighbours(index: Int) = {
    val x = index % 10
    val y = index / 10

    Seq(
      (x - 1, y - 1),
      (x + 0, y - 1),
      (x + 1, y - 1),
      (x - 1, y),
      (x + 1, y),
      (x - 1, y + 1),
      (x + 0, y + 1),
      (x + 1, y + 1)
    ).filter { case (x, y) => x >= 0 && x < 10 && y >= 0 && y < 10 }
      .map { case (x, y) => x + 10 * y }
  }

  private def loadMap(stream: Iterator[String]) =
    stream.flatMap(_.split("").map(_.toInt)).toArray

  private def printMap(array: Array[Int]) =
    "\n" + String.join("\n", array.mkString.grouped(10).toSeq: _*)
