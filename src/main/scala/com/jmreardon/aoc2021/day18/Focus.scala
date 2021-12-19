package com.jmreardon.aoc2021.day18

import scala.annotation.tailrec

/*
  This file implements a zipper API for accessing a SnailNumber.
  This helps support performing the 'explosion' operation, since
  it requires us to navigate back and forth to find numbers.
 */

// Representation of the context of the traversal
sealed trait Context:
  def depth: Int = 0

// At the top of the tree
case object Top extends Context

// On the left side of a tree branch
case class LeftBranch(right: SnailNumber, up: Context) extends Context:
  override def depth: Int = 1 + up.depth

// On the right side of the tree branch
case class RightBranch(left: SnailNumber, up: Context) extends Context:
  override def depth: Int = 1 + up.depth

/*
 * A class to encapsulate traversal of snail numbers. It provides functions to
 * perform back and forward depth-first left-first traversal of the tree,
 * matching the directions needed to perform the 'explode' operation.
 *
 * If an operation would leave the tree, the traversal is considered complete
 * and the method returns the full SnailNumber.
 */
case class Focus(value: SnailNumber, context: Context = Top):
  def update(newValue: SnailNumber) = Focus(newValue, context)

  // To to the top of the tree and get the full value.
  @tailrec
  final def top: SnailNumber = up match
    case Left(result) => result
    case Right(above) => above.top

  def next: Either[SnailNumber, Focus] =
    left.orElse(right).fold(nextUp)(Right(_))

  def prev: Either[SnailNumber, Focus] =
    context match
      case Top           => Left(value)
      case _: LeftBranch => up
      case _: RightBranch =>
        up.flatMap(_.left.fold(Left(top))(Right(_))).map(_.rightMost)

  private def rightMost: Focus =
    right.orElse(left).map(_.rightMost).getOrElse(this)

  private def nextUp: Either[SnailNumber, Focus] =
    context match
      case Top            => Left(value)
      case _: LeftBranch  => up.flatMap(_.right.fold(Left(top))(Right(_)))
      case _: RightBranch => up.flatMap(_.nextUp)

  private def up: Either[SnailNumber, Focus] =
    context match
      case Top                   => Left(value)
      case LeftBranch(right, up) => Right(Focus(Pair(value, right), up))
      case RightBranch(left, up) => Right(Focus(Pair(left, value), up))

  private def left: Option[Focus] =
    value match
      case Pair(l, r) => Some(Focus(l, LeftBranch(r, context)))
      case _          => None

  private def right: Option[Focus] =
    value match
      case Pair(l, r) => Some(Focus(r, RightBranch(l, context)))
      case _          => None
