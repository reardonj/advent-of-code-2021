package com.jmreardon.aoc2021.day21

import com.jmreardon.aoc2021.Day
import cats.data.State

object Day21 extends Day:
  def a(stream: Iterator[String]): Any =
    val player1 :: player2 :: Nil = parseStartingPositions(stream)
    val result @ (die, player1Done, player2Done) =
      playDeterministicGame(DeterministicDie(0), player1, player2)
    die.timesRolled * Seq(player1Done, player2Done).map(_.score).min

  def b(stream: Iterator[String]): Any =
    val player1 :: player2 :: Nil = parseStartingPositions(stream)
    playDiracGames(DiracDice(0), player1, player2)
      .run(Map())
      .value
      ._1((player1, player2))

  private def playDeterministicGame(
      die: DeterministicDie,
      player1: Player,
      player2: Player
  ): (DeterministicDie, Player, Player) =
    val (nextDie, roll) = die.roll
    val updatedPlayer1 = player1.takeTurn(roll)
    if updatedPlayer1.score >= 1000 then (nextDie, updatedPlayer1, player2)
    else playDeterministicGame(nextDie, player2, updatedPlayer1)

  private type Victories = Map[Int, Long]
  private type MemoizedVictories = Map[(Player, Player), Victories]

  private def playDiracGames(
      die: DiracDice,
      player1: Player,
      player2: Player
  ): State[MemoizedVictories, Victories] =
    for
      existingSolution <- State.inspect[MemoizedVictories, Option[Victories]](
        _.get((player1, player2))
      )
      updated <- existingSolution match
        case Some(victories) => State.pure(victories)
        case None =>
          val (nextDie, rolls) = die.roll
          sumGames(nextDie, rolls, Map(), player1, player2)
    yield updated

  private def sumGames(
      die: DiracDice,
      rolls: List[Int],
      total: Victories,
      player1: Player,
      player2: Player
  ): State[MemoizedVictories, Victories] = rolls match
    case Nil => State(memo => (memo + ((player1, player2) -> total), total))
    case x :: xs =>
      val nextPlayer1 = player1.takeTurn(x)
      if nextPlayer1.score >= 21 then
        val updatedTotal = mergeInto(total, Map(nextPlayer1.id -> 1L))
        sumGames(die, xs, updatedTotal, player1, player2)
      else
        for
          subGameTotals <- playDiracGames(die, player2, nextPlayer1)
          updatedTotal = mergeInto(total, subGameTotals)
          subGameResult <- sumGames(die, xs, updatedTotal, player1, player2)
        yield subGameResult

  private def mergeInto(map: Map[Int, Long], toMerge: Map[Int, Long]) =
    toMerge.foldLeft(map) { case (mergingInto, (k, v)) =>
      mergingInto.updatedWith(k)(currentVal =>
        Some(currentVal.getOrElse(0L) + v)
      )
    }

  private case class Player(id: Int, position: Int, score: Int):
    def takeTurn(move: Int) =
      val nextPosition = (position + move) % 10
      this.copy(position = nextPosition, score = score + nextPosition + 1)
    override def toString() = s"Player$id at $position with score $score"

  private case class DeterministicDie(timesRolled: Int):
    def roll = (
      DeterministicDie(timesRolled + 3),
      (0 to 2).map(_ + timesRolled).map(_ % 100 + 1).sum
    )

  // All possible rolls from 3d3:
  private val diracRolls = (for
    a <- 1 to 3
    b <- 1 to 3
    c <- 1 to 3
  yield a + b + c).toList

  private case class DiracDice(timesRolled: Int):
    def roll = (DiracDice(timesRolled + 3), diracRolls)

  private def parseStartingPositions(stream: Iterator[String]): List[Player] =
    stream
      .flatMap(_.split(": ").lastOption)
      .map(_.toInt - 1)
      .zipWithIndex
      .map { case (position, id) => Player(id + 1, position, 0) }
      .toList
