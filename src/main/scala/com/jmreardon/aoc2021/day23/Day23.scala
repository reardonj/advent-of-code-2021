package com.jmreardon.aoc2021.day23

import com.jmreardon.aoc2021.Day
import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec

object Day23 extends Day:
  def a(stream: Iterator[String]): Any =
    val scenario = Scenario(2)
    val initial = scenario.parse(stream)
    println(initial)
    dijkstra(initial).map(_._1)

  def b(stream: Iterator[String]): Any =
    val scenario = Scenario(4)
    val modified = stream.take(1) ++ Seq("DCBA", "DBAC") ++ stream.take(1)
    val initial = scenario.parse(modified)
    println(initial)
    dijkstra(initial).map(_._1)

  private case class Pos(x: Int, y: Int):
    override def toString() = s"$x,$y"

  enum State:
    case Unmoved, MovingOut, Waiting, MovingIn, Done
  private class Scenario(roomDepth: Int):

    def parse(input: Iterator[String]): GameState =
      GameState(
        initialPositions
          .zip(input.flatMap(_.split("")))
          .map { case (pos, id) =>
            val kind = idToKind(id)
            ActorState(kind, pos, kind.stateForPos(pos))
          }
          .toSet
      )

    private def idToKind(id: String) = id match
      case "A" => Kind.A
      case "B" => Kind.B
      case "C" => Kind.C
      case "D" => Kind.D
      case _   => ???

    enum Kind:
      case A, B, C, D
      def stateForPos(position: Pos) = (this, position) match
        case (A, Pos(2, 2)) => State.Done
        case (B, Pos(4, 2)) => State.Done
        case (C, Pos(6, 2)) => State.Done
        case (D, Pos(8, 2)) => State.Done
        case _              => State.Unmoved

      def energy = this match
        case A => 1
        case B => 10
        case C => 100
        case D => 1000

      def destinationX =
        val x = this match
          case A => 2
          case B => 4
          case C => 6
          case D => 8
        (1 to roomDepth).toSeq.map(Pos(x, _))

    private val initialPositions: Seq[Pos] =
      for
        y <- 1 to roomDepth
        x <- Seq(2, 4, 6, 8)
      yield Pos(x, y)

    private val hallPositions = Set.from(0 to 10).map(x => Pos(x, 0))
    private val stopPositions =
      hallPositions -- Set(2, 4, 6, 8).map(x => Pos(x, 0))
    private val legalPositions = hallPositions ++ initialPositions

    case class ActorState(kind: Kind, position: Pos, state: State):
      override def toString() = s"($position)$kind,$state"

    case class GameState(
        actors: Set[ActorState]
    ):

      def aStarHeuristic =
        actors.toSeq
          .filterNot(_.state == State.Done)
          .map { actor =>
            (1 + Math.abs(
              actor.kind.destinationX(0).x - actor.position.x
            )) * actor.kind.energy
          }
          .sum
      def done: Boolean = actors.forall(_.state == State.Done)
      def occupied(position: Pos) = actors.exists(_.position == position)

      def nextStates: Seq[(Int, GameState)] =
        nextMovingInStates
          .orElse(nextMovingOutStates)
          .getOrElse(nextUnmovedStates ++ nextWaitingStates)

      private def update(
          oldState: ActorState,
          newState: ActorState
      ) =
        this.copy(actors = actors - oldState + newState)

      private def move(oldState: ActorState, newState: ActorState) =
        (newState.kind.energy, update(oldState, newState))

      private def nextUnmovedStates =
        actors
          .filter(_.state == State.Unmoved)
          .toSeq
          .flatMap { actor =>
            outPoints(actor.position)
              .intersect(legalPositions)
              .filterNot(occupied)
              .toSeq
              .map { pos =>
                move(actor, actor.copy(position = pos, state = State.MovingOut))
              }
          }

      private def nextMovingOutStates =
        actors
          .find(_.state == State.MovingOut)
          .map { actor =>
            outPoints(actor.position)
              .intersect(legalPositions)
              .filterNot(occupied)
              .toSeq
              .flatMap { pos =>
                val nextMove = move(actor, actor.copy(position = pos))
                if stopPositions.contains(pos) then
                  Seq(
                    nextMove,
                    move(
                      actor,
                      actor.copy(position = pos, state = State.Waiting)
                    )
                  )
                else Seq(nextMove)
              }
          }

      private def nextMovingInStates: Option[Seq[(Int, GameState)]] =
        actors.find(_.state == State.MovingIn).map { actor =>
          nextMovingInMoves(actor.kind, actor.position).toSeq
            .map { pos =>
              val next = move(actor, actor.copy(position = pos))
              val nextMoves = next._2.nextMovingInMoves(actor.kind, pos)
              if nextMoves.isEmpty && actor.kind.destinationX.contains(pos) then
                move(actor, actor.copy(position = pos, state = State.Done))
              else next
            }
        }

      private def nextMovingInMoves(kind: Kind, position: Pos) =
        // Check if it can stop.
        val destinations = kind.destinationX
        val destintationX = destinations(0).x
        (hallPositions ++ destinations)
          .intersect(inPoints(position))
          .filterNot(occupied)
          .filter(move =>
            Math.abs(move.x - destintationX) <= Math
              .abs(position.x - destintationX)
          )

      private def nextWaitingStates =
        actors
          .filter(_.state == State.Waiting)
          .toSeq
          .flatMap { actor =>
            nextMovingInMoves(actor.kind, actor.position).toSeq
              .map { pos =>
                move(actor, actor.copy(position = pos, state = State.MovingIn))
              }
          }
          .toSeq

  private def inPoints(point: Pos) = point match {
    case Pos(x, y) =>
      Set(
        Pos(x - 1, y),
        Pos(x + 1, y),
        Pos(x, y + 1)
      )
  }

  private def outPoints(point: Pos) = point match {
    case Pos(x, y) =>
      Set(
        Pos(x - 1, y),
        Pos(x + 1, y),
        Pos(x, y - 1)
      )
  }

  private def dijkstra(
      start: Scenario#GameState
  ): Option[(Int, Scenario#GameState)] =
    // val cameFrom =
    //   scala.collection.mutable.Map[Scenario#GameState, Scenario#GameState]()
    val gScore =
      scala.collection.mutable.Map(start -> 0).withDefaultValue(Int.MaxValue)
    val visited = scala.collection.mutable.Set[Scenario#GameState]()

    val openSet = scala.collection.mutable
      .SortedMap(start.aStarHeuristic -> Set(start))

    while (openSet.nonEmpty) {
      val (k, minSet) = openSet.head
      val current = minSet.head
      if minSet.size == 1 then openSet.remove(k)
      else openSet.update(k, minSet - current)

      if current.done then
        // println(
        //   addToPath(start, current, cameFrom, Nil)
        //     .map(s => (cost(s), s))
        //     .mkString("\n")
        // )
        return Some((gScore(current), current))
      else
        visited.add(current)
        if visited.size % 100000 == 0 then
          println(
            s"Checked ${visited.size}, queued: ${openSet.foldLeft(0)(_ + _._2.size)}"
          )
        // println("At: " + current)
        // println("Neighbours: ")
        // println(current.nextStates.mkString("\n\t", "\n\t", ""))
        for {
          (weight, neighbour) <- current.nextStates
          if !visited.contains(neighbour)
        } {
          val oldCost = gScore(neighbour)
          val newCost = gScore(current) + weight
          if newCost < oldCost then
            // cameFrom(neighbour) = current
            val h = neighbour.aStarHeuristic
            gScore(neighbour) = newCost
            openSet.updateWith(oldCost + h)(
              _.map(_ - neighbour).flatMap(set =>
                Option.unless(set.isEmpty)(set)
              )
            )
            openSet.updateWith(newCost + h)(
              _.map(_ + neighbour).orElse(Some(Set(neighbour)))
            )
        }
    }

    None

  @tailrec
  private def addToPath(
      from: Scenario#GameState,
      to: Scenario#GameState,
      cameFromGraph: Scenario#GameState => Scenario#GameState,
      path: List[Scenario#GameState]
  ): List[Scenario#GameState] =
    if from == to then path
    else
      val prev = cameFromGraph(to)
      addToPath(from, prev, cameFromGraph, to :: path)
