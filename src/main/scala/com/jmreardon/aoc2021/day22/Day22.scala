package com.jmreardon.aoc2021.day22

import com.jmreardon.aoc2021.Day
import cats.data.State

object Day22 extends Day:
  def a(stream: Iterator[String]): Any =
    val maxRegion = -131072 to 131072
    val baseTree: OctTree =
      EmptyOctTree(1, Region(maxRegion, maxRegion, maxRegion))
    parse(stream)
      .takeWhile(_.region.containedBy(Region(-50 to 50, -50 to 50, -50 to 50)))
      .foldLeft(baseTree) {
        case (tree, RegionCommand(true, region))  => tree.add(region)
        case (tree, RegionCommand(false, region)) => tree.remove(region)
      }
      .volume

  def b(stream: Iterator[String]): Any = 0

  /*
   Need a different algorithm
   Start with no regions
     - on adds the region to the list
     - off does nothing
   With one or more regions
     - on add,
       - find the intersection between the region and each existing region and add a negating region
       - add the region
     - on a remove, intersect with each region and add a negating operation
   */

  private def rangeTotal(a: Range, b: Range) =
    Math.min(a.start, b.start) to Math.max(a.end, b.end)

  private trait OctTree:
    val depth: Int
    val region: Region
    def volume: BigInt
    def add(cube: Region): OctTree
    def remove(cube: Region): OctTree

  private case class EmptyOctTree(depth: Int, region: Region) extends OctTree:
    def volume: BigInt = BigInt(0)
    def add(cube: Region): OctTree =
      cube.within(region) match
        case None         => this
        case Some(within) => LeafOctTree(depth, region, within)

    def remove(cube: Region): OctTree = this

  private case class LeafOctTree(depth: Int, region: Region, on: Region)
      extends OctTree:

    def volume: BigInt = on.volume

    def add(cube: Region): OctTree =
      cube.within(region) match
        case None => this
        case Some(toAdd) =>
          if toAdd.containedBy(on) then this
          else BranchOctTree(depth, region, split.map(_.add(toAdd)))

    def remove(cube: Region): OctTree =
      cube.within(region) match
        case None => this
        case Some(toRemove) =>
          if on.containedBy(toRemove) then EmptyOctTree(depth, region)
          else BranchOctTree(depth, region, split.map(_.remove(toRemove)))

    private def split = region.split.map { subRegion =>
      //if depth < 10 then println(s"Splitting $this")
      on.within(subRegion) match
        case None        => EmptyOctTree(depth + 1, subRegion)
        case Some(subOn) => LeafOctTree(depth + 1, subRegion, subOn)
    }

  private case class BranchOctTree(
      depth: Int,
      region: Region,
      children: Seq[OctTree]
  ) extends OctTree:
    def volume: BigInt = children.map(_.volume).sum

    def add(cube: Region): OctTree =
      cube.within(region) match
        case None => this
        case Some(toAdd) =>
          if region.containedBy(toAdd) then LeafOctTree(depth, region, region)
          else this.copy(children = children.map(_.add(toAdd)))

    def remove(cube: Region): OctTree =
      cube.within(region) match
        case None => this
        case Some(toRemove) =>
          if region.containedBy(toRemove) then EmptyOctTree(depth, region)
          else
            val updatedChildren = children.map(_.remove(toRemove))
            if updatedChildren.collect { case EmptyOctTree(_, _) =>
                1
              }.sum == 8
            then EmptyOctTree(depth, region)
            else this.copy(children = updatedChildren)

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

    private def containedBy(a: Range, b: Range) =
      b.start <= a.start && b.end >= a.end

    def intersects(that: Region): Boolean =
      intersects(x, that.x) && intersects(y, that.y) && intersects(z, that.z)

    def within(that: Region): Option[Region] =
      val bound = Region(
        boundRange(x, that.x),
        boundRange(y, that.y),
        boundRange(z, that.z)
      )
      Option.unless(bound.volume == 0)(bound)

    private def boundRange(range: Range, bounds: Range) =
      Math.max(range.start, bounds.start) to Math.min(range.end, bounds.end)

    def split: Seq[Region] =
      for
        xSplit <- splitRange(x)
        ySplit <- splitRange(y)
        zSplit <- splitRange(z)
      yield Region(xSplit, ySplit, zSplit)

    private def intersects(a: Range, b: Range) =
      b.contains(a.start) || b.contains(a.end) ||
        a.contains(b.start) || a.contains(b.end)

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
