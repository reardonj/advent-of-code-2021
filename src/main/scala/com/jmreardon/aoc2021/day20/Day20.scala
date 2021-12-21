package com.jmreardon.aoc2021.day20

import com.jmreardon.aoc2021.Day

object Day20 extends Day:
  def a(stream: Iterator[String]): Any =
    parseInput(stream).enhance.enhance.lit

  def b(stream: Iterator[String]): Any =
    (1 to 50).foldLeft(parseInput(stream))((map, _) => map.enhance).lit

  private case class Image(
      enhanceAlgorithm: Map[Int, Int],
      initialized: Map[(Int, Int), Int],
      min: Int,
      max: Int,
      uninitializedOn: Int
  ):
    def lit = initialized.values.count(_ == 1)
    def enhance =
      val newMap = for
        x <- (min - 1) to (max + 1)
        y <- (min - 1) to (max + 1)
      yield (x, y) -> enhancePixel(x, y)

      val nextUninitializedState = enhanceAlgorithm(
        if uninitializedOn == 1 then 511 else 0
      )
      Image(
        enhanceAlgorithm,
        newMap.toMap.withDefaultValue(nextUninitializedState),
        min - 1,
        max + 1,
        nextUninitializedState
      )

    override def toString() =
      val map =
        (min to max)
          .map(y =>
            (min to max)
              .map(x => if initialized((x, y)) == 1 then '#' else ' ')
              .mkString
          )
          .mkString("\n")
      s"min = $min; max = $max; uninitializedOn = $uninitializedOn\n" + map

    private def enhancePixel(x: Int, y: Int): Int =
      enhanceAlgorithm(
        (initialized((x - 1, y - 1)) << 8) |
          (initialized((x, y - 1)) << 7) |
          (initialized((x + 1, y - 1)) << 6) |
          (initialized((x - 1, y)) << 5) |
          (initialized((x, y)) << 4) |
          (initialized((x + 1, y)) << 3) |
          (initialized((x - 1, y + 1)) << 2) |
          (initialized((x, y + 1)) << 1) |
          (initialized((x + 1, y + 1)))
      )

  private def parseInput(stream: Iterator[String]): Image =
    val transform =
      stream.next.zipWithIndex.collect {
        case ('#', i) => i -> 1
        case (_, i)   => (i -> 0)
      }.toMap
    val image = (for
      (line, y) <- stream.drop(1).zipWithIndex
      (char, x) <- line.zipWithIndex
    yield (x, y) -> (if char == '#' then 1 else 0)).toSeq

    Image(
      transform,
      image.toMap.withDefaultValue(0),
      min = 0,
      max = image.last._1._1,
      uninitializedOn = 0
    )
