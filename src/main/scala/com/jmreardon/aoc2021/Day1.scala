package com.jmreardon.aoc2021

object Day1 extends Day:

  // https://adventofcode.com/2021/day/1
  def a(stream: Iterator[String]): Int =
    stream
      .map(_.toInt)
      .sliding(2, 1)
      .filter { case Seq(prev, next) => next > prev }
      .length

  // https://adventofcode.com/2021/day/1#part2
  def b(stream: Iterator[String]): Int =
    stream
      .map(_.toInt)
      .sliding(3)
      .map(_.sum)
      .sliding(2, 1)
      .filter { case Seq(prev, next) => next > prev }
      .length
