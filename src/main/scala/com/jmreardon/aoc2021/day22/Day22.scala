package com.jmreardon.aoc2021.day22

import com.jmreardon.aoc2021.Day
import cats.data.State

object Day22 extends Day:
  def a(stream: Iterator[String]): Any =
    parse(stream)
      .takeWhile(_.region.containedBy(Region(-50 to 50, -50 to 50, -50 to 50)))
      .foldLeft(Seq[(Int, Region)]())(handleCommand)
      .map { case (mod, region) => region.volume * mod }
      .sum

  def b(stream: Iterator[String]): Any =
    parse(stream)
      //.takeWhile(_.region.containedBy(Region(-50 to 50, -50 to 50, -50 to 50)))
      .foldLeft(Seq[(Int, Region)]())(handleCommand)
      .map { case (mod, region) => region.volume * mod }
      .sum

  private def handleCommand(
      regions: Seq[(Int, Region)],
      command: RegionCommand
  ): Seq[(Int, Region)] = command match
    case RegionCommand(true, region) =>
      regions ++ regions.flatMap(addNegation(region, _)) :+ (1, region)
    case RegionCommand(false, region) =>
      regions ++ regions.flatMap(addNegation(region, _))

  private def addNegation(
      region: Region,
      toNegate: (Int, Region)
  ): Option[(Int, Region)] =
    val (mod, regionToNegate) = toNegate
    region.within(regionToNegate).map(intersection => (-1 * mod, intersection))

  private case class Region(x: Range, y: Range, z: Range):
    def volume = BigInt(x.length) * BigInt(y.length) * BigInt(z.length)

    def within(that: Region): Option[Region] =
      val bound = Region(
        boundRange(x, that.x),
        boundRange(y, that.y),
        boundRange(z, that.z)
      )
      Option.unless(bound.volume == 0)(bound)

    private def boundRange(range: Range, bounds: Range) =
      Math.max(range.start, bounds.start) to Math.min(range.end, bounds.end)

  private case class RegionCommand(on: Boolean, region: Region)

  private def parse(stream: Iterator[String]): Iterator[RegionCommand] =
    val parser =
      """(\w*) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)""".r
    stream.map(line =>
      line match {
        case parser(isOn, x1, x2, y1, y2, z1, z2) =>
          RegionCommand(
            isOn == "on",
            Region(
              x1.toInt to x2.toInt,
              y1.toInt to y2.toInt,
              z1.toInt to z2.toInt
            )
          )
      }
    )
