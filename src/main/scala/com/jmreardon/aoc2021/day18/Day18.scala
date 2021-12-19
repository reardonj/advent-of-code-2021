package com.jmreardon.aoc2021.day18

import com.jmreardon.aoc2021.Day
import cats.data.State
import scala.annotation.tailrec

/*
   Solution for Say 18 of Advent of Code 2021:
   https://adventofcode.com/2021/day/18
 */

object Day18 extends Day:
  def a(stream: Iterator[String]): Any =
    stream.map[SnailNumber](readNumber).reduce(_ + _).magnitude

  def b(stream: Iterator[String]): Any =
    val values = stream.map[SnailNumber](readNumber).toSeq
    val magnitudes =
      for
        a <- values
        b <- values
        if a != b
      yield (a + b).magnitude
    magnitudes.max

  // Parse input using a simple State monad based parser.
  private def readNumber(input: String) = parsePair.run(input.toList).value._2

  private val parseSingle =
    // Consume as many digits as possible
    State((s: List[Char]) => s.span(_.isDigit).swap)
      .map(_.mkString.toInt)
      .map[SnailNumber](Single(_))

  private val parsePair: State[List[Char], Pair] =
    for
      _ <- mustConsume('[')
      left <- parseSnailNumber
      _ <- mustConsume(',')
      right <- parseSnailNumber
      _ <- mustConsume(']')
    yield Pair(left, right)

  private lazy val parseSnailNumber: State[List[Char], SnailNumber] =
    for
      isPair <- isNext('[')
      num <- if isPair then parsePair else parseSingle
    yield num

  private def isNext(c: Char): State[List[Char], Boolean] =
    State(stream => (stream, stream.head == c))

  // Enforce that a character must be present.
  private def mustConsume(c: Char): State[List[Char], Unit] =
    State(stream =>
      if stream.head == c then (stream.tail, ())
      else
        throw IllegalArgumentException(
          s"Invalid number, missing '$c' at: $stream"
        )
    )
