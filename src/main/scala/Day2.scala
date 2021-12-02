import fs2._

object Day2:
  // https://adventofcode.com/2021/day/2
  def a[F[_]]: Pipe[F, String, Int] = stream =>
    stream
      .map(directionToVector)
      .fold((0, 0)) { case ((ax, ay), (bx, by)) => (ax + bx, ay + by) }
      .map { case (x, y) => x * y }

  // https://adventofcode.com/2021/day/2#part2
  def b[F[_]]: Pipe[F, String, Int] = stream =>
    stream
      .map(directionToCommand)
      .fold(SubState(0, 0, 0))((state, command) => command.execute(state))
      .map(state => state.x * state.y)

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

case class SubState(aim: Int, x: Int, y: Int)

sealed trait SubCommand:
  def execute(state: SubState): SubState

case class Forward(amount: Int) extends SubCommand:
  def execute(state: SubState): SubState =
    state.copy(x = state.x + amount, y = state.y + state.aim * amount)

case class Down(amount: Int) extends SubCommand:
  def execute(state: SubState): SubState =
    state.copy(aim = state.aim + amount)

case class Up(amount: Int) extends SubCommand:
  def execute(state: SubState): SubState =
    state.copy(aim = state.aim - amount)
