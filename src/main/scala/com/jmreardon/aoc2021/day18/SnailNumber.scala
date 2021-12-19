package com.jmreardon.aoc2021.day18

import scala.annotation.tailrec

sealed trait SnailNumber:
  def +(that: SnailNumber): SnailNumber = Pair(this, that).reduce
  def magnitude: Int

case class Single(magnitude: Int) extends SnailNumber:
  override def toString() = magnitude.toString()

case class Pair(left: SnailNumber, right: SnailNumber) extends SnailNumber:
  override def toString() = s"[$left,$right]"
  def reduce = performReduce(Focus(this))
  def magnitude = 3 * left.magnitude + 2 * right.magnitude

  @tailrec
  private def performReduce(focus: Focus): SnailNumber =
    tryExplode(focus) orElse trySplit(focus) match
      case Some(num) => performReduce(Focus(num))
      case None      => focus.value

  @tailrec
  private def tryExplode(focus: Focus): Option[SnailNumber] =
    focus.value match
      case Pair(Single(l), Single(r)) if focus.context.depth == 4 =>
        val propagatedBackwards = propagate(l, focus.update(Single(0)), _.prev)
        Some(propagate(r, findHole(propagatedBackwards), _.next).top)
      case _ =>
        focus.next match
          case Left(result)     => None
          case Right(nextFocus) => tryExplode(nextFocus)

  @tailrec
  private def trySplit(focus: Focus): Option[SnailNumber] =
    focus.value match
      case Single(value) if value >= 10 =>
        val split = Pair(Single(value / 2), Single((value + 1) / 2))
        Some(focus.update(split).top)
      case _ =>
        focus.next match
          case Left(result)     => None
          case Right(nextFocus) => trySplit(nextFocus)

  private def findHole(focus: Focus): Focus =
    focus.value match
      case Single(0) => focus
      case _ =>
        focus.next match
          case Right(next) => findHole(next)
          case _ =>
            throw new IllegalArgumentException("0 from explosion missing")

  @tailrec
  private def propagate(
      exploded: Int,
      focus: Focus,
      direction: Focus => Either[SnailNumber, Focus]
  ): Focus = direction(focus) match
    case Left(_) => focus
    case Right(next) =>
      next.value match
        case Single(v) => next.update(Single(v + exploded))
        case _         => propagate(exploded, next, direction)
