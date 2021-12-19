package com.jmreardon.aoc2021.day19

import scala.annotation.tailrec
import com.jmreardon.aoc2021.Day

object Day19 extends Day:
  def a(stream: Iterator[String]): Any =
    merge(parse(stream)).beacons.size

  def b(stream: Iterator[String]): Any =
    val scanners = merge(parse(stream)).scanners
    scanners
      .flatMap(a => scanners.map(b => (a, b)))
      .map(offsetTo(_, _).manhattan)
      .max

  // Merge beacons of aligned scanners into a single view.
  private def merge(scanners: Seq[Scanner]) =
    val aligned = alignAll(scanners)
    SensorMap(
      aligned.map(_.beacons.toSet).reduce(_.union(_)),
      aligned.map(_.position).toSet
    )

  // Align a group of scanners so all beacon and scanner positions
  // are rotated and offset correctly.
  private def alignAll(scanners: Seq[Scanner]): Seq[Scanner] =
    alignRecursively(Seq(scanners.head), scanners.tail, Seq())

  /** Aligns scanners by:
    *
    *   - Finding the unaligned scanners that can be aligned with one of the
    *     last aligned scanners.
    *
    *   - Moving the already aligned scanners to the done set. We have just
    *     found every scanner that can be aligned with it, so this avoids
    *     pointless scans.
    *
    *   - Repeating the process, using the newly aligned scanners as the new
    *     last aligned set, until there are no unaligned scanners.
    */
  @tailrec
  private def alignRecursively(
      lastAligned: Seq[Scanner],
      unaligned: Seq[Scanner],
      done: Seq[Scanner]
  ): Seq[Scanner] =
    val (remaining, found) = unaligned
      .map(scanner =>
        // Use an iterator to only find the first match
        lastAligned.iterator
          .flatMap(findRotations(_, scanner).nextOption)
          .nextOption
          .toRight(scanner)
      )
      .partitionMap(identity)
    if remaining.isEmpty then found ++ lastAligned ++ done
    else alignRecursively(found, remaining, lastAligned ++ done)

  // Find the rotations of b that match a.
  private def findRotations(a: Scanner, b: Scanner): Iterator[Scanner] =
    for
      orientation <- b.orientations
      offset <- findOffsetsMatch(a.beacons, orientation.beacons)
    yield Scanner(
      b.id,
      orientation.beacons.map(add(_, offset)),
      add(orientation.position, offset)
    )

  // Try to enough matching offsets between a and b that we can infer some
  // beacons are the same.
  private def findOffsetsMatch(a: Seq[Beacon], b: Seq[Beacon]): Option[Offset] =
    val iterator = b.iterator.flatMap(beacon => a.map(offsetTo(_, beacon)))

    // This is the application inner loop, use mutable map for efficiency
    val counts = scala.collection.mutable.Map[Offset, Int]()
    // Check at each step so we can exit the search early if we found enough offsets.
    @tailrec
    def loop: Option[Offset] = iterator.nextOption match
      case None => None
      case Some(next) =>
        counts
          .updateWith(next)(v => Some(v.getOrElse(0) + 1))
          .filter(_ == 12) match
          case None    => loop
          case Some(_) => Some(next)

    loop

  case class Beacon(x: Int, y: Int, z: Int)

  case class Offset(x: Int, y: Int, z: Int):
    def manhattan = x.abs + y.abs + z.abs

  private def offsetTo(the: Beacon, that: Beacon): Offset =
    Offset(the.x - that.x, the.y - that.y, the.z - that.z)
  private def add(the: Beacon, offset: Offset) =
    Beacon(the.x + offset.x, the.y + offset.y, the.z + offset.z)

  private case class SensorMap(beacons: Set[Beacon], scanners: Set[Beacon])

  private case class Scanner(
      id: String,
      beacons: Seq[Beacon] = Seq(),
      position: Beacon = Beacon(0, 0, 0)
  ):
    def add(beacon: Beacon) = this.copy(beacons = beacons :+ beacon)

    def orientations = orientationTransforms.iterator.map(transform =>
      copy(beacons = beacons.map(transform))
    )

  private val orientationTransforms =
    def turn(b: Beacon): Beacon = Beacon(-b.y, b.x, b.z)
    def roll(b: Beacon): Beacon = Beacon(b.x, b.z, -b.y)
    for
      majorRotation <- Iterator
        .iterate(identity[Beacon], 2)(
          roll compose turn compose roll compose _
        )
        .toSeq
      rolled <- Iterator.iterate(majorRotation, 3)(roll compose _)
      turned <- Iterator.iterate(rolled, 4)(turn compose _)
    yield turned

  private def parse(stream: Iterator[String]): Seq[Scanner] =
    @tailrec
    def parseStream(scanners: Seq[Scanner]): Seq[Scanner] =
      stream.nextOption match
        case None => scanners
        case Some(header) =>
          parseStream(scanners :+ parseScanner(Scanner(header.split(" ")(2))))

    @tailrec
    def parseScanner(scanner: Scanner): Scanner =
      stream.nextOption.map(_.split(",")) match
        case Some(a) if a.length < 3 => scanner
        case Some(Array(x, y, z)) =>
          parseScanner(scanner.add(Beacon(x.toInt, y.toInt, z.toInt)))
        case _ => scanner

    parseStream(List())
