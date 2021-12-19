package com.jmreardon.aoc2021

import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue
import scala.concurrent.duration.DurationConversions.fromNowConvert
object Day15 extends Day:
  private type Cell = (Int, Int)
  def a(stream: Iterator[String]): Any =
    val weights = loadMap(stream)
    val lookup = weights.toMap
    val graph = aStar(lookup, weights.head._1, weights.last._1)

    addToPath(weights.head._1, weights.last._1, graph, Nil).map(lookup).sum

  def b(stream: Iterator[String]): Any =
    val weights = expandCells(loadMap(stream))
    val lookup = weights.toMap

    val graph = aStar(lookup, weights.head._1, weights.last._1)

    addToPath(weights.head._1, weights.last._1, graph, Nil).map(lookup).sum

  private def expandCells(cells: Seq[(Cell, Int)]) =
    cells.flatMap(expandCell(cells.last._1._1 + 1))

  private def expandCell(expandBy: Int)(entry: (Cell, Int)): Seq[(Cell, Int)] =
    val (cell, weight) = entry
    val xOrigin = cell._1
    val yOrigin = cell._2
    for {
      xOffset <- 0 until 5
      yOffset <- 0 until 5
      yWeight = offsetWeight(weight, yOffset + xOffset)
    } yield {
      (xOrigin + expandBy * xOffset, yOrigin + expandBy * yOffset) -> yWeight
    }

  private def offsetWeight(weight: Int, offset: Int) =
    val total = (weight + offset) % 9
    if total == 0 then 9 else total

  @tailrec
  private def addToPath(
      from: Cell,
      to: Cell,
      cameFromGraph: Cell => Cell,
      path: List[Cell]
  ): List[Cell] =
    if from == to then path
    else
      val prev = cameFromGraph(to)
      addToPath(from, prev, cameFromGraph, to :: path)

  private def aStar(
      weights: Map[Cell, Int],
      start: Cell,
      goal: Cell
  ): Function1[Cell, Cell] =
    val cameFrom = scala.collection.mutable.Map[Cell, Cell]()
    val cost = scala.collection.mutable.Map[Cell, Int](start -> 0)

    val inOpenSet = scala.collection.mutable.Set(start)
    val queue =
      PriorityQueue((start, cost(start)))(
        Ordering.by[(Cell, Int), Int](_._2).reverse
      )

    while (queue.nonEmpty) {
      val (current, _) = queue.dequeue
      if current == goal then queue.clear
      else
        inOpenSet -= current
        for {
          neighbour <- neighbours(current)
          weight <- weights.get(neighbour)
        } {
          val newCost =
            cost.getOrElse(current, Int.MaxValue) + weight
          if newCost < cost.getOrElse(neighbour, Int.MaxValue) then
            cameFrom(neighbour) = current
            cost(neighbour) = newCost
            if inOpenSet.add(neighbour) then queue.enqueue((neighbour, newCost))
        }
    }

    cameFrom

  private def neighbours(
      cell: Cell
  ): Seq[Cell] = {
    val (x, y) = cell

    Seq(
      (x + 0, y - 1),
      (x - 1, y),
      (x + 1, y),
      (x + 0, y + 1)
    )
  }

  private def loadMap(stream: Iterator[String]): Seq[(Cell, Int)] =
    stream.zipWithIndex.flatMap { case (row, y) =>
      row.split("").map(_.toInt).zipWithIndex.map { case (weight, x) =>
        (x, y) -> weight
      }
    }.toIndexedSeq
