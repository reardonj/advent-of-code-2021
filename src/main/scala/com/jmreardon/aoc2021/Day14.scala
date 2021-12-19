package com.jmreardon.aoc2021

import scala.annotation.tailrec
object Day14 extends Day:

  def a(stream: Iterator[String]): Any =
    val (polymer, insertions) = readStream(stream)

    val result = polymerizeAndCount(10, polymer, insertions)
    val elementsByFrequency = result.toSeq.sortBy(_._2)
    elementsByFrequency.last._2 - elementsByFrequency.head._2

  def b(stream: Iterator[String]): Any =
    val (polymer, insertions) = readStream(stream)

    val result = polymerizeAndCount(40, polymer, insertions)
    val elementsByFrequency = result.toSeq.sortBy(_._2)
    elementsByFrequency.last._2 - elementsByFrequency.head._2

  private def readStream(stream: Iterator[String]) =
    val polymer = stream.next.toCharArray.toSeq
    // consume blank line
    stream.next
    val insertions = stream
      .map(_.split(" -> ") match {
        case Array(pair, i) => (pair(0), pair(1)) -> i(0)
      })
      .toMap
    (polymer, insertions)

  private case class Expansion(steps: Int, pair: (Char, Char))

  private def polymerizeAndCount(
      steps: Int,
      polymer: Seq[Char],
      insertions: Map[(Char, Char), Char]
  ): Map[Char, Long] =
    val pairs = polymer
      .sliding(2, 1)
      .map { case Seq(a, b) => Expansion(steps, (a, b)) }
      .toList
    val solutions = polymerizeQueue(insertions, Map(), pairs)
    pairs.foldLeft(Map[Char, Long](polymer.last -> 1))((total, pair) =>
      mergeCounts(total, solutions(pair))
    )

  @tailrec
  private def polymerizeQueue(
      insertions: Map[(Char, Char), Char],
      solutions: Map[Expansion, Map[Char, Long]],
      queue: List[Expansion]
  ): Map[Expansion, Map[Char, Long]] = queue match {
    case Nil => solutions
    case (expansion @ Expansion(0, (a, _))) :: rest =>
      polymerizeQueue(
        insertions,
        solutions + (expansion -> Map(a -> 1L)),
        rest
      )
    case (e @ Expansion(steps, (a, b))) :: rest =>
      solutions.get(e) match {
        case Some(solution) =>
          polymerizeQueue(insertions, solutions, rest)
        case None =>
          val expanded = insertions((a, b))
          val left = Expansion(steps - 1, (a, expanded))
          val right = Expansion(steps - 1, (expanded, b))
          solutions
            .get(left)
            .flatMap(l => solutions.get(right).map(r => (l, r))) match {
            case Some((leftSolution, rightSolution)) =>
              polymerizeQueue(
                insertions,
                solutions + (e -> mergeCounts(leftSolution, rightSolution)),
                rest
              )
            case None =>
              polymerizeQueue(insertions, solutions, left :: right :: e :: rest)
          }
      }

  }

  private def mergeCounts(a: Map[Char, Long], b: Map[Char, Long]) =
    a.foldLeft(b) { case (map, (key, value)) =>
      map.updated(key, map.getOrElse(key, 0L) + value)
    }
