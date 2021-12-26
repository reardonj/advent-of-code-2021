package com.jmreardon.aoc2021.day23

import com.jmreardon.aoc2021.Day
import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec

object Day23 extends Day:
  def a(stream: Iterator[String]): Any =
    val scenario = Scenario(2)
    val initial = scenario.parse(stream)
    println(initial)
    aStar(initial).map(_._1)

  def b(stream: Iterator[String]): Any =
    val scenario = Scenario(4)
    val modified = stream.take(1) ++ Seq("DCBA", "DBAC") ++ stream.take(1)
    val initial = scenario.parse(modified)
    println(initial)
    aStar(initial).map(_._1)

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

    enum Kind(val energy: Int, val destination: Int):
      case A extends Kind(1, 2)
      case B extends Kind(10, 4)
      case C extends Kind(100, 6)
      case D extends Kind(1000, 8)

      def stateForPos(position: Pos) = (this, position) match
        case (A, Pos(2, `roomDepth`)) => State.Done
        case (B, Pos(4, `roomDepth`)) => State.Done
        case (C, Pos(6, `roomDepth`)) => State.Done
        case (D, Pos(8, `roomDepth`)) => State.Done
        case _                        => State.Unmoved

      def destinations =
        (1 to roomDepth).toSeq.map(Pos(destination, _))

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

      val aStarHeuristic =
        actors.toSeq.map { case ActorState(kind, Pos(x, y), state) =>
          val base = Math.abs(kind.destination - x) + 1
          val total = state match
            case State.Done      => 0
            case State.Unmoved   => base + y
            case State.MovingOut => base + y
            case _               => base
          total * kind.energy
        }.sum

      def done: Boolean = actors.forall(_.state == State.Done)
      def occupied(position: Pos) = actors.exists(_.position == position)

      def nextStates: Seq[(Int, GameState)] =
        nextKeepMovingStates.getOrElse(nextStartMovingStates)

      private def update(oldState: ActorState, newState: ActorState) =
        this.copy(actors = actors - oldState + newState)

      private def move(oldState: ActorState, pos: Pos, state: State) =
        (
          oldState.kind.energy,
          update(oldState, oldState.copy(position = pos, state = state))
        )

      private def nextKeepMovingStates =
        actors.collectFirst {
          case actor @ ActorState(_, _, State.MovingOut) =>
            movingOutStatesFor(actor)
          case actor @ ActorState(_, _, State.MovingIn) =>
            nextMovingInMoves(actor.kind, actor.position).toSeq
              .map { pos =>
                val next = move(actor, pos, State.MovingIn)
                if actor.kind.destinations.contains(pos) && next._2
                    .nextMovingInMoves(actor.kind, pos)
                    .isEmpty
                then move(actor, pos, State.Done)
                else next
              }
        }

      private def movingOutStatesFor(actor: ActorState) =
        outPoints(actor.position)
          .intersect(legalPositions)
          .filterNot(occupied)
          .toSeq
          .flatMap { pos =>
            val nextMove = move(actor, pos, State.MovingOut)
            if stopPositions.contains(pos) then
              Seq(nextMove, move(actor, pos, State.Waiting))
            else Seq(nextMove)
          }

      private def nextMovingInMoves(kind: Kind, position: Pos) =
        (hallPositions ++ kind.destinations)
          .intersect(inPoints(position))
          .filterNot(occupied)
          .filter(move =>
            val destintationX = kind.destination
            Math.abs(move.x - destintationX) <= Math
              .abs(position.x - destintationX)
          )

      private def nextStartMovingStates =
        actors.toSeq.collect {
          case actor @ ActorState(_, _, State.Unmoved) =>
            movingOutStatesFor(actor)
          case actor @ ActorState(_, _, State.Waiting) =>
            val destinations = actor.kind.destinations.toSet
            // If only the same kind of actor is in the room, start, otherwise don't.
            if actors
                .filter(a => destinations.contains(a.position))
                .forall(_.kind == actor.kind)
            then
              nextMovingInMoves(actor.kind, actor.position).toSeq
                .map(move(actor, _, State.MovingIn))
            else Seq()
        }.flatten

  private def inPoints(point: Pos) =
    Set(
      Pos(point.x - 1, point.y),
      Pos(point.x + 1, point.y),
      Pos(point.x, point.y + 1)
    )

  private def outPoints(point: Pos) =
    Set(
      Pos(point.x - 1, point.y),
      Pos(point.x + 1, point.y),
      Pos(point.x, point.y - 1)
    )

  private def aStar(
      start: Scenario#GameState
  ): Option[(Int, Scenario#GameState)] =
    val gScore =
      scala.collection.mutable.Map(start -> 0).withDefaultValue(Int.MaxValue)
    val visited = scala.collection.mutable.Set[Scenario#GameState]()

    val queue =
      PriorityQueue((start, start.aStarHeuristic))(
        Ordering.by[(Scenario#GameState, Int), Int](_._2).reverse
      )

    while (queue.nonEmpty) {
      val current = queue.dequeue._1
      if current.done then return Some((gScore(current), current))
      else if visited.add(current) then
        if visited.size % 100000 == 0 then
          println(
            s"Checked ${visited.size}, queued: ${queue.size}"
          )
        val currentGScore = gScore(current)
        for {
          (weight, neighbour) <- current.nextStates
          if !visited.contains(neighbour)
        } {
          val oldCost = gScore(neighbour)
          val newCost = currentGScore + weight
          if newCost < oldCost then
            val h = neighbour.aStarHeuristic
            gScore(neighbour) = newCost
            queue.enqueue((neighbour, newCost + h))
        }
    }

    None
