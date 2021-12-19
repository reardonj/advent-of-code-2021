package com.jmreardon.aoc2021

object Day4 extends Day:
  type Board = Seq[Seq[Option[Int]]]

  def a(stream: Iterator[String]): Any =
    findWinners(stream).head

  def b(stream: Iterator[String]): Any =
    findWinners(stream).last

  private def findWinners(stream: Iterator[String]): Seq[Int] = {
    val (numbers, boards) = readData(stream)
    numbers
      // List boards at each step
      .scanLeft(boards)((boards, called) =>
        applyNumber(boards.filterNot(boardIsSolved), called)
      )
      // Except the initial state
      .drop(1)
      // Then find the score of the first winner each round
      .zip(numbers)
      .map { case (winners, called) =>
        winners
          .filter(boardIsSolved)
          .headOption
          .map(_.flatten.flatten.sum * called)
      }
      // Just return the winning scores, in order
      .flatten
  }

  private def applyNumber(boards: Seq[Board], number: Int): Seq[Board] =
    boards.map(_.map(_.map(_.filter(_ != number))))

  private def boardIsSolved(board: Board): Boolean =
    board.exists(_.forall(_.isEmpty))
      || board.transpose.exists(_.forall(_.isEmpty))

  private def readData(stream: Iterator[String]): (Seq[Int], Seq[Board]) = {
    val numbers = stream.next.split(",").map(_.toInt)
    val boards = stream
      .grouped(6)
      .map(_.drop(1).map(_.trim.split("\\s+").map(x => x.toIntOption).toSeq))
      .toSeq
    (numbers, boards)
  }
