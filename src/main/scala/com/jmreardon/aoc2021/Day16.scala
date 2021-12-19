package com.jmreardon.aoc2021

import cats.data.State
object Day16 extends Day:
  def a(stream: Iterator[String]): Any =
    stream.map(readPacket).map(sumVersions).toSeq

  def b(stream: Iterator[String]): Any =
    stream.map(readPacket).map(evaluate).toSeq

  private case class Version(v: Int)
  private case class Packet(version: Version, content: PacketContent)

  private sealed trait PacketContent
  private case class Literal(value: BigInt) extends PacketContent
  private case class Operator(typeId: Int, subPackets: Seq[Packet])
      extends PacketContent

  private def sumVersions(packet: Packet): Int =
    packet.version.v + (packet.content match {
      case Literal(_)           => 0
      case Operator(_, packets) => packets.map(sumVersions).sum
    })

  private def evaluate(packet: Packet): BigInt = packet.content match {
    case Literal(lit)           => lit
    case Operator(5, Seq(a, b)) => if evaluate(a) > evaluate(b) then 1 else 0
    case Operator(6, Seq(a, b)) => if evaluate(a) < evaluate(b) then 1 else 0
    case Operator(7, Seq(a, b)) => if evaluate(a) == evaluate(b) then 1 else 0
    case Operator(op, packets) =>
      val values = packets.map(evaluate)
      op match {
        case 0 => values.sum
        case 1 => values.product
        case 2 => values.min
        case 3 => values.max
      }
  }

  private def readPacket(hexString: String) =
    val input = hexString
      .split("")
      .toList
      .map(Integer.parseInt(_, 16).toBinaryString)
      .flatMap(padNibble)
    parsePacket.run(input).value._2

  private def padNibble(nibble: String) =
    String.join("", Array.fill(4 - nibble.length)("0"): _*) + nibble

  private val parseVersion = parseBits(3).map(Version.apply)
  private val parseContent = parseBits(3).flatMap(parseType)
  private val parsePacket = for {
    version <- parseVersion
    content <- parseContent
  } yield Packet(version, content)

  private def parseType(packetType: Int): State[List[Char], PacketContent] =
    if packetType == 4 then parseLiteral() else parseOperator(packetType)

  private def parseLiteral(
      content: String = ""
  ): State[List[Char], PacketContent] =
    for {
      consumeMore <- consumeOne.map(_ == '1')
      nibble <- consume(4).map(_.mkString)
      total <-
        if consumeMore then parseLiteral(content + nibble)
        else State.pure(Literal(BigInt(content + nibble, 2)))
    } yield total

  private def parseOperator(operator: Int): State[List[Char], PacketContent] =
    for {
      flag <- consumeOne
      content <- if flag == '0' then parseByLength else parseByCount
    } yield Operator(operator, content)

  private val parseByCount = parseBits(11).flatMap(repeat(_, parsePacket))

  private val parseByLength = for {
    length <- parseBits(15)
    subPacketData <- consume(length)
  } yield parsePackets.run(subPacketData).value._2

  private val parsePackets: State[List[Char], List[Packet]] =
    for {
      next <- parsePacket
      hasMore <- State.inspect[List[Char], Boolean](_.nonEmpty)
      rest <- if hasMore then parsePackets else State.pure(Nil)
    } yield next :: rest

  private def repeat[A](
      n: Int,
      parser: State[List[Char], A]
  ): State[List[Char], List[A]] =
    if n == 0 then State.pure(Nil)
    else
      for {
        next <- parser
        rest <- repeat(n - 1, parser)
      } yield next :: rest

  private def consume(n: Int): State[List[Char], List[Char]] =
    State(stream => stream.splitAt(n).swap)

  private val consumeOne: State[List[Char], Char] =
    State(stream => (stream.tail, stream.head))

  private def parseBits(n: Int) =
    consume(n).map(_.mkString).map(Integer.parseInt(_, 2))
