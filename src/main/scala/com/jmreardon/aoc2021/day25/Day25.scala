package com.jmreardon.aoc2021.day25

import com.jmreardon.aoc2021.Day

object Day25 extends Day:
  def a(stream: Iterator[String]): Any = stepUntilStop(parse(stream))

  def b(stream: Iterator[String]): Any = 0

  private case class Pos(x: Int, y: Int)

  private enum Cucumber:
    case Right, Down

  private case class SeaFloor(
      xMax: Int,
      yMax: Int,
      cucumbers: Map[Pos, Cucumber]
  ):
    override def toString() =
      val cells = for
        y <- 0 to yMax
        x <- 0 to xMax
      yield cucumbers
        .get(Pos(x, y))
        .map {
          case Cucumber.Down  => "v"
          case Cucumber.Right => ">"
        }
        .getOrElse(".")
      cells.grouped(xMax + 1).map(_.mkString).mkString("\n", "\n", "\n \n")

  private def stepUntilStop(state: SeaFloor, steps: Int = 1): Int =
    val nextState = takeStep(state)
    if nextState == state then steps else stepUntilStop(nextState, steps + 1)

  private def takeStep(state: SeaFloor): SeaFloor =
    val afterRight = stepIf(state, Cucumber.Right, 1, 0)
    stepIf(afterRight, Cucumber.Down, 0, 1)

  private def stepIf(
      state: SeaFloor,
      toMove: Cucumber,
      xMove: Int,
      yMove: Int
  ) =
    state.copy(cucumbers = state.cucumbers.map { case (pos, cucumber) =>
      if cucumber != toMove then (pos, cucumber)
      else
        val nextPos =
          Pos(
            (pos.x + xMove) % (state.xMax + 1),
            (pos.y + yMove) % (state.yMax + 1)
          )
        if state.cucumbers.contains(nextPos) then (pos, cucumber)
        else (nextPos, cucumber)
    })

  private def parse(
      stream: Iterator[String],
      state: SeaFloor = SeaFloor(-1, -1, Map())
  ): SeaFloor =
    stream.nextOption match
      case None => state
      case Some(line) =>
        val y = state.yMax + 1
        val cucumbers = line.zipWithIndex.collect {
          case ('v', x) => (Pos(x, y), Cucumber.Down)
          case ('>', x) => (Pos(x, y), Cucumber.Right)
        }
        parse(
          stream,
          state.copy(
            xMax = Math.max(state.xMax, line.length - 1),
            yMax = y,
            cucumbers = state.cucumbers ++ cucumbers
          )
        )
