package com.jmreardon.aoc2021.day24

import com.jmreardon.aoc2021.Day
import cats.data.State
import scala.util.Try
import scala.util.Success

object Day24 extends Day:
  def a(stream: Iterator[String]): Any =
    val program = stream.map(parse).toSeq
    findValidInput(Seq(9, 8, 7, 6, 5, 4, 3, 2, 1), 0, AluState(), program)

  def b(stream: Iterator[String]): Any =
    val program = stream.map(parse).toSeq
    findValidInput(
      Seq(9, 8, 7, 6, 5, 4, 3, 2, 1).reverse,
      0,
      AluState(),
      program
    )

  private def findValidInput(
      digits: Seq[Int],
      depth: Int,
      state: AluState,
      program: Seq[Instruction],
      failures: Set[(AluState, Int)] = Set()
  ): Either[String, Set[(AluState, Int)]] =
    executeProgram(state, program) match
      case (newState, Some(needsInput)) =>
        if failures.contains((newState, depth)) then Right(failures)
        else
          digits
            .foldLeft[Either[String, Set[(AluState, Int)]]](Right(failures)) {
              (result, next) =>
                result.fold(
                  Left(_),
                  findValidInput(
                    digits,
                    depth + 1,
                    newState,
                    needsInput(next),
                    _
                  ).left.map(next.toString + _)
                )
            }
            .map(_ + ((newState, depth)))
      case (result, None) =>
        Either.cond(result.z != 0, failures + ((state, depth)), "")

  private def executeProgram(
      state: AluState,
      program: Seq[Instruction]
  ): (AluState, Option[Long => Seq[Instruction]]) =
    program.headOption.map(_.execute(state)) match
      case None => (state, None)
      case Some(Left(needsInput)) =>
        (
          // The program resets these anyways, so reset myself to reduce states.
          state.copy(w = 0, x = 0, y = 0),
          Some((x: Long) => needsInput(x) +: program.tail)
        )
      case Some(Right(newState)) => executeProgram(newState, program.tail)

  private sealed trait Arg:
    def resolve(state: AluState): Long

  private object Arg:
    def unapply(x: String): Option[Arg] =
      Reg.unapply(x).orElse(x.toLongOption.map(Const(_)))

  private case class Const(c: Long) extends Arg:
    def resolve(state: AluState): Long = c

  private enum Reg extends Arg:
    case W, X, Y, Z
    def resolve(state: AluState): Long = this match
      case Reg.W => state.w
      case Reg.X => state.x
      case Reg.Y => state.y
      case Reg.Z => state.z
    def set(state: AluState, to: Long): AluState = this match
      case Reg.W => state.copy(w = to)
      case Reg.X => state.copy(x = to)
      case Reg.Y => state.copy(y = to)
      case Reg.Z => state.copy(z = to)

  private object Reg:
    def unapply(x: String): Option[Reg] = x match
      case "w" => Some(Reg.W)
      case "x" => Some(Reg.X)
      case "y" => Some(Reg.Y)
      case "z" => Some(Reg.Z)
      case _   => None

  private case class AluState(
      w: Long = 0,
      x: Long = 0,
      y: Long = 0,
      z: Long = 0
  )

  private sealed trait Instruction:
    def execute(state: AluState): Either[Long => Instruction, AluState]

  private case class Inp(a: Reg) extends Instruction:
    def execute(state: AluState) =
      Left(input => Assign(a, Const(input)))

  private case class Assign(a: Reg, b: Arg) extends Instruction:
    def execute(state: AluState) =
      Right(a.set(state, b.resolve(state)))

  private case class Add(a: Reg, b: Arg) extends Instruction:
    def execute(state: AluState) =
      Right(a.set(state, a.resolve(state) + b.resolve(state)))

  private case class Mul(a: Reg, b: Arg) extends Instruction:
    def execute(state: AluState) =
      Right(a.set(state, a.resolve(state) * b.resolve(state)))

  private case class Div(a: Reg, b: Arg) extends Instruction:
    def execute(state: AluState) =
      Right(a.set(state, a.resolve(state) / b.resolve(state)))

  private case class Mod(a: Reg, b: Arg) extends Instruction:
    def execute(state: AluState) =
      Right(a.set(state, a.resolve(state) % b.resolve(state)))

  private case class Eql(a: Reg, b: Arg) extends Instruction:
    def execute(state: AluState) =
      Right(a.set(state, if a.resolve(state) == b.resolve(state) then 1 else 0))

  private def parse(line: String): Instruction = line.split(" ") match
    case Array("inp", Reg(a))         => Inp(a)
    case Array("add", Reg(a), Arg(b)) => Add(a, b)
    case Array("mul", Reg(a), Arg(b)) => Mul(a, b)
    case Array("div", Reg(a), Arg(b)) => Div(a, b)
    case Array("mod", Reg(a), Arg(b)) => Mod(a, b)
    case Array("eql", Reg(a), Arg(b)) => Eql(a, b)
