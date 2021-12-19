package com.jmreardon.aoc2021

object Day8 extends Day:
  def a(stream: Iterator[String]): Any =
    stream
      .flatMap(_.split(" \\| ")(1).split(" "))
      .toSeq
      .groupMapReduce(_.length)(_ => 1)(_ + _)
      .filterKeys(key => key == 2 || key == 4 || key == 3 || key == 7)
      .values
      .sum

  def b(stream: Iterator[String]): Any =
    stream.filterNot(_.isBlank).map(extractLine).sum

  private def extractLine(line: String): Int = {
    val Array(ten, output) = line
      .split(" \\| ")
      .map(_.split(" ").map(_.map(digitToBinary).sum))

    val tenBySegments = ten.groupBy(Integer.bitCount)
    val one = tenBySegments(2)(0)
    val four = tenBySegments(4)(0)
    val seven = tenBySegments(3)(0)
    val eight = tenBySegments(7)(0)

    val six = tenBySegments(6).find(n => (n & one) != one).get
    val nine = tenBySegments(6).find(n => (n & four) == four).get
    val zero = tenBySegments(6).diff(Seq(six, nine)).head

    val three = tenBySegments(5).find(n => (n & one) == one).get
    val two = tenBySegments(5).find(n => (n | nine) != nine).get
    val five = tenBySegments(5).diff(Seq(two, three)).head

    val binaryToDigit = Map(
      zero -> 0,
      one -> 1,
      two -> 2,
      three -> 3,
      four -> 4,
      five -> 5,
      six -> 6,
      seven -> 7,
      eight -> 8,
      nine -> 9
    )

    output.map(binaryToDigit).map(_.toString).fold("")(_ + _).toInt
  }

  private val digitToBinary = "abcdefg".zipWithIndex.map { case (k, v) =>
    (k, 1 << v)
  }.toMap
