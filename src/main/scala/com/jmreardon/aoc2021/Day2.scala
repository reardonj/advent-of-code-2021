package com.jmreardon.aoc2021

object Day2 extends Day:
  // https://adventofcode.com/2021/day/2
  def a(stream: Iterator[String]): Int =
    stream
      .map(directionToVector)
      .fold((0, 0)) { case ((ax, ay), (bx, by)) => (ax + bx, ay + by) } match {
      case (x, y) => x * y
    }

  // https://adventofcode.com/2021/day/2#part2
  def b(stream: Iterator[String]): Int =
    stream
      .map(directionToCommand)
      .foldLeft(SubState(0, 0, 0))((state, command) => command.execute(state))
      .value

  private def directionToVector(line: String): (Int, Int) =
    line.split(" ") match {
      case Array("forward", x) => (x.toInt, 0)
      case Array("down", x)    => (0, x.toInt)
      case Array("up", x)      => (0, -x.toInt)
    }

  private def directionToCommand(line: String): SubCommand =
    line.split(" ") match {
      case Array("forward", x) => Forward(x.toInt)
      case Array("down", x)    => Down(x.toInt)
      case Array("up", x)      => Up(x.toInt)
    }

case class SubState(aim: Int, x: Int, y: Int):
  def add(aim: Int = 0, x: Int = 0, y: Int = 0) =
    copy(aim = this.aim + aim, x = this.x + x, y = this.y + y)

  def value: Int = x * y

sealed trait SubCommand:
  def execute(state: SubState): SubState

case class Forward(amount: Int) extends SubCommand:
  def execute(state: SubState): SubState =
    state.add(x = amount, y = state.aim * amount)

case class Down(amount: Int) extends SubCommand:
  def execute(state: SubState): SubState =
    state.add(aim = amount)

case class Up(amount: Int) extends SubCommand:
  def execute(state: SubState): SubState =
    state.add(aim = -amount)
