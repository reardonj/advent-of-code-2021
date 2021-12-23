package com.jmreardon.aoc2021.day22

import com.jmreardon.aoc2021.Day
import cats.data.State

object Day22 extends Day:
  def a(stream: Iterator[String]): Any =
    parse(stream)
      .takeWhile(_.region.containedBy(Region(-50 to 50, -50 to 50, -50 to 50)))
      .foldLeft[Seq[Region]](Seq()) {
        case (regions, RegionCommand(true, region)) =>
          add(region, regions)
        case (regions, RegionCommand(false, region)) =>
          remove(region, regions)
      }
      .map(_.volume)
      .sum

  def b(stream: Iterator[String]): Any =
    val maxRegion = -131072 to 131072
    val baseTree: OctTree =
      OctTreeNode(Region(maxRegion, maxRegion, maxRegion), false)
    val commands = parse(stream).foldLeft(baseTree) {
      case (tree, RegionCommand(on, region)) => tree.set(on, region)
    }

  private def rangeTotal(a: Range, b: Range) =
    Math.min(a.start, b.start) to Math.max(a.end, b.end)

  private trait OctTree:
    val region: Region
    def set(to: Boolean, cube: Region): OctTree

  private case class OctTreeBranch(region: Region, children: Seq[OctTree])
      extends OctTree:
    def set(to: Boolean, cube: Region): OctTree =
      if region.containedBy(cube) then OctTreeNode(region, to)
      else if region.intersects(cube) then
        val updatedChildren = children.map(_.set(to, cube))
        val setChildren = updatedChildren
          .collect { case OctTreeNode(_, x) => x }
          .count(_ == to)
        if setChildren == 8 then OctTreeNode(region, to)
        else OctTreeBranch(region, updatedChildren)
      else this

  private case class OctTreeNode(region: Region, on: Boolean) extends OctTree:
    def set(to: Boolean, cube: Region): OctTree =
      if region.containedBy(cube) then this.copy(on = to)
      else if region.intersects(cube) then
        val children = region.split.map(r => OctTreeNode(r, on))
        OctTreeBranch(region, children).set(to, cube)
      else this

  private def add(
      toAdd: Region,
      regions: Seq[Region]
  ): Seq[Region] = remove(toAdd, regions) :+ toAdd

  private def remove(
      toRemove: Region,
      regions: Seq[Region],
      processed: Seq[Region] = Nil
  ): Seq[Region] = regions match {
    case Seq(x, xs @ _*) =>
      if x.containedBy(toRemove) then
        // println(s"$x contained by $toRemove, removing")
        remove(toRemove, xs, processed)
      else if x.intersects(toRemove) then
        // println(s"$x intersects $toRemove, splitting")
        remove(toRemove, x.split ++ xs, processed)
      else
        // if processed.length % 10000 == 9 || xs.length % 10000 == 0 then
        //   println(s"${xs.length} left, ${1 + processed.length} processed")
        remove(toRemove, xs, processed :+ x)
    case Seq() => processed
  }

  private case class Region(x: Range, y: Range, z: Range):
    def volume = BigInt(x.length) * BigInt(y.length) * BigInt(z.length)

    def containedBy(that: Region): Boolean =
      containedBy(x, that.x) && containedBy(y, that.y) && containedBy(z, that.z)

    def intersects(that: Region): Boolean =
      intersects(x, that.x) && intersects(y, that.y) && intersects(z, that.z)

    def split: Seq[Region] =
      for
        xSplit <- splitRange(x)
        ySplit <- splitRange(y)
        zSplit <- splitRange(z)
      yield Region(xSplit, ySplit, zSplit)

    private def intersects(a: Range, b: Range) =
      b.contains(a.start) || b.contains(a.end) ||
        a.contains(b.start) || a.contains(b.end)

    private def containedBy(a: Range, b: Range) =
      b.contains(a.start) && b.contains(a.end)

    private def splitRange(range: Range): Seq[Range] =
      splitRangeAt(range, range.length / 2)

    private def splitRangeAt(range: Range, at: Int): Seq[Range] =
      val (a, b) = range.splitAt(at)
      Seq(a, b).filterNot(_.isEmpty)

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
